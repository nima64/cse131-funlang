// ...existing code...
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};
use std::sync::atomic::{AtomicU64, Ordering};
use crate::types::{TypeInfo, Defn};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompileState {
    NotCompiled,
    FastAvailable,
    OnlySlow,
}

#[derive(Debug)]
pub struct FunctionMeta {
    pub state: CompileState,
    pub fast_addr: Option<u64>,
    pub call_count: u64,
}

impl Default for FunctionMeta {
    fn default() -> Self {
        FunctionMeta {
            state: CompileState::NotCompiled,
            fast_addr: None,
            call_count: 0,
        }
    }
}


#[derive(Clone, Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub arg_names: Vec<String>,
    pub arg_types: Vec<Option<TypeInfo>>,
    // heap-allocated mutable metadata for runtime/JIT to update
    pub meta: Arc<Mutex<FunctionMeta>>,
}

static NEXT_FN_ID: AtomicU64 = AtomicU64::new(1);

static REGISTRY: OnceLock<Mutex<HashMap<u64, Arc<FunctionInfo>>>> = OnceLock::new();

static NAME_TO_ID: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();

fn name_index() -> &'static Mutex<HashMap<String, u64>> {
    NAME_TO_ID.get_or_init(|| Mutex::new(HashMap::new()))
}

fn registry() -> &'static Mutex<HashMap<u64, Arc<FunctionInfo>>> {
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn register_function(info: FunctionInfo) -> u64 {
    let id = NEXT_FN_ID.fetch_add(1, Ordering::Relaxed);
    let mut map = registry().lock().expect("registry mutex poisoned");
    map.insert(id, Arc::new(info));
    id
}

pub fn record_function(defn: &Defn) -> u64 {
    let fname = defn.name.clone();

    // Fast-path: check if name already has an id.
    let mut name_map = name_index().lock().expect("name_index mutex poisoned");
    if let Some(&existing_id) = name_map.get(&fname) {
        return existing_id;
    }

    let info = FunctionInfo {
        name: defn.name.clone(),
        arg_names: defn.params.iter().map(|(n, _)| n.clone()).collect(),
        arg_types: defn.params.iter().map(|(_, t)| t.clone()).collect(),
        meta: Arc::new(Mutex::new(FunctionMeta::default())),
    };
    let id = register_function(info);
    name_map.insert(fname, id);
    id
}

pub fn unregister_function(id: u64) {
    let mut map = registry().lock().expect("registry mutex poisoned");
    map.remove(&id);
}

#[no_mangle]
pub extern "C" fn compile_me(id: u64) -> i32 {
    let map = registry().lock().expect("registry mutex poisoned");
    if let Some(info) = map.get(&id) {
        // Print header to stderr so we don't interfere with program stdout
        eprintln!("compile_me: fn id={} name=\"{}\"", id, info.name);
        // Print each argument name with its optional TypeInfo
        for (i, name) in info.arg_names.iter().enumerate() {
            let ty_str = info
                .arg_types
                .get(i)
                .and_then(|opt| opt.as_ref())
                .map(|t| format!("{:?}", t))
                .unwrap_or_else(|| "<unknown>".to_string());
            eprintln!("  arg {}: {} : {}", i, name, ty_str);
        }

        // Print and update mutable metadata
        if let Ok(mut meta) = info.meta.lock() {
            // Show current state and fast_addr
            eprintln!("  state: {:?}", meta.state);
            if let Some(addr) = meta.fast_addr {
                eprintln!("  fast_addr: 0x{:x}", addr);
            } else {
                eprintln!("  fast_addr: <none>");
            }

            // Example use of mutable state: print different messages on first/second calls
            if meta.call_count == 0 {
                eprintln!("  message: First call to {}!", info.name);
            } else {
                eprintln!("  message: Called {} again! (count={})", info.name, meta.call_count + 1);
            }

            // Increment call count
            meta.call_count = meta.call_count.saturating_add(1);
        } else {
            eprintln!("  meta: <lock error>");
        }

        0
    } else {
        eprintln!("compile_me: unknown fn={}", id);
        -1
    }
}
