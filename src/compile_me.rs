// ...existing code...
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicU64, Ordering};
use dynasmrt::AssemblyOffset;
use crate::types::{TypeInfo, Defn, is_number_tag, is_bool_tag, is_subtype};
use crate::typechecker::type_check;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

pub static GLOBAL_OPS: OnceLock<Mutex<dynasmrt::Assembler<dynasmrt::x64::X64Relocation>>> = OnceLock::new();

pub static mut FAST_ADDRS: [u64; 1024] = [0; 1024];

pub fn get_global_ops() -> &'static Mutex<dynasmrt::Assembler<dynasmrt::x64::X64Relocation>> {
    GLOBAL_OPS.get_or_init(|| Mutex::new(dynasmrt::x64::Assembler::new().unwrap()))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum CompileState {
    NotCompiled,
    FastAvailable,
    OnlySlow,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct FunctionInfo {
    pub name: String,
    pub defn: Defn,
    
    // Merged fields from FunctionMeta
    pub state: CompileState,
    pub fast_addr: Option<u64>,
    pub call_count: u64,
    pub recorded_args: Option<Vec<TypeInfo>>,
}

static NEXT_FN_ID: AtomicU64 = AtomicU64::new(1);

static REGISTRY: OnceLock<Mutex<HashMap<u64, FunctionInfo>>> = OnceLock::new();

static NAME_TO_ID: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();

fn name_index() -> &'static Mutex<HashMap<String, u64>> {
    NAME_TO_ID.get_or_init(|| Mutex::new(HashMap::new()))
}

fn registry() -> &'static Mutex<HashMap<u64, FunctionInfo>> {
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn register_function(info: FunctionInfo) -> u64 {
    let id = NEXT_FN_ID.fetch_add(1, Ordering::Relaxed);
    let mut map = registry().lock().expect("registry mutex poisoned");
    map.insert(id, info);
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
        defn: defn.clone(),
        state: CompileState::NotCompiled,
        fast_addr: None,
        call_count: 0,
        recorded_args: None,
    };
    let id = register_function(info);
    name_map.insert(fname, id);
    id
}

#[no_mangle]
pub extern "C" fn print_fast_msg() {
    //TODO: replace with real faster version
    eprintln!("this is fast version");
}

#[no_mangle]
pub extern "C" fn compile_me(id: u64, args_ptr: *const i64, count: u64, slow_path_addr: u64) -> u64 {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compile_me_inner(id, args_ptr, count, slow_path_addr)
    }));

    match result {
        Ok(val) => val,
        Err(e) => {
            if let Some(s) = e.downcast_ref::<&str>() {
                eprintln!("Runtime Error: {}", s);
            } else if let Some(s) = e.downcast_ref::<String>() {
                eprintln!("Runtime Error: {}", s);
            } else {
                eprintln!("Runtime Error: Unknown panic");
            }
            std::process::exit(1);
        }
    }
}
fn compile_me_inner(
    id: u64,
    args_ptr: *const i64,
    count: u64,
    slow_path_addr: u64,
) -> u64 {
    let mut map = registry().lock().unwrap();
    
    if !map.contains_key(&id) {
        return 0;
    }

    let call_count = map.get(&id).unwrap().call_count;

    /* ============================================
       Case 1: First call — MUST type-check
    ============================================ */
    if call_count == 0 {
        // Collect all definitions for type checking (requires immutable borrow of map)
        let defns: Vec<Defn> = map.values().map(|info| info.defn.clone()).collect();
        
        // Now get mutable reference to info
        let info = map.get_mut(&id).unwrap();

        // Read raw argument values
        let args = unsafe { std::slice::from_raw_parts(args_ptr, count as usize) };

        // Infer argument types from tags
        let mut inferred_types: Vec<TypeInfo> = Vec::new();
        for &val in args {
            let t = if is_number_tag(val) {
                TypeInfo::Num
            } else if is_bool_tag(val) {
                TypeInfo::Bool
            } else {
                TypeInfo::Any
            };
            inferred_types.push(t);
        }


        // Check arity
        if info.defn.params.len() != inferred_types.len() {
           
            info.state = CompileState::OnlySlow;
            info.call_count = 1;
            return 0;   // must still use slow version
        }

        // Build type env
        let mut env_t = im::HashMap::new();
        for ((param_name, _), tp) in info.defn.params.iter().zip(&inferred_types) {
            env_t.insert(param_name.clone(), Box::new(tp.clone()));
        }

        let mut define_env_t = HashMap::new();

        // Run type_check: may panic → handled by caller of this function
        let typed_body = type_check(&info.defn.body, &env_t, &mut define_env_t, &defns);

        // If function declared a return type, ensure compatible
        if let Some(expected) = &info.defn.return_type {
            let actual = typed_body.get_type_info();
            if !is_subtype(actual, expected) {
                info.state = CompileState::OnlySlow;
                info.call_count = 1;
                return 0;
            }
        }

        // All good — so generate fast version
        info.recorded_args = Some(inferred_types.clone());
        info.state = CompileState::FastAvailable;


        // ----------- Fast code generation ---------------------
        let mut ops_guard = get_global_ops().lock().unwrap();
        let mut ops = std::mem::replace(&mut *ops_guard, dynasmrt::x64::Assembler::new().unwrap());

        let print_fast_ptr = print_fast_msg as *const ();
        let slow_addr_i64 = slow_path_addr as i64;
        
        // Generate type guards
        for (i, arg_type) in inferred_types.iter().enumerate() {
            let offset = (16 + i * 8) as i32;  // [rbp + 16], [rbp + 24], ...

            dynasm!(ops
                ; .arch x64
                ; mov rax, [rbp + offset]
                ; test rax, 1
            );

            match arg_type {
                TypeInfo::Num => {         // expect tag 0 → (LSB == 0)
                    dynasm!(ops ; jnz ->slow);
                },
                TypeInfo::Bool => {       // expect tag 1 → (LSB == 1)
                    dynasm!(ops ; jz ->slow);
                },
                _ => {} // ANY → no guard
            }
        }

        // If passes all guards, jump into slow for now (we don't inline body!)
        
        dynasm!(ops
            ; .arch x64
            ; push rbp
            ; mov rbp, rsp
            ; and rsp, -16
            ; mov rax, QWORD print_fast_ptr as i64
            ; call rax
            ; mov rsp, rbp
            ; pop rbp
            ; jmp ->slow
            ; ->slow:
            ; mov rax, QWORD slow_addr_i64
            ; jmp rax
        );
        //TODO: when optimization finished, use this version
        //  dynasm!(ops
        //     ; .arch x64
        //     ; push rbp
        //     ; mov rbp, rsp
        //     ; and rsp, -16
        //     ; mov rax, QWORD print_fast_ptr as i64
        //     ; call rax
        //     ; mov rsp, rbp
        //     ; pop rbp
        //     ; ->slow:
        //     ; mov rax, QWORD slow_addr_i64
        //     ; jmp rax
        // );

        // Commit code
        match ops.finalize() {
            Ok(buf) => {
                let fast_ptr = buf.ptr(AssemblyOffset(0)) as u64;
                info.fast_addr = Some(fast_ptr);
                std::mem::forget(buf);
            }
            Err(e) => {
                eprintln!("  assembler error: {:?}", e);
                info.state = CompileState::OnlySlow;
            }
        }

        info.call_count = 1;

        /* ************************************************************
           IMPORTANT RULE OF THE ASSIGNMENT:
           First call ALWAYS returns 0 → stub uses slow path for THIS CALL
        ************************************************************ */
        return 0;
    }

    /* ============================================
       Case 2: Later calls
    ============================================ */
    let info = map.get_mut(&id).unwrap();
    info.call_count += 1;

    match info.state {
        CompileState::OnlySlow => {
            // Always slow
            return 0;
        }
        CompileState::FastAvailable => {
            let fast = info.fast_addr.unwrap_or(0);
            if fast == 0 {
                return 0; // fallback
            }

            return fast;
        }
        CompileState::NotCompiled => unreachable!("cannot reach here"),
    }
}
