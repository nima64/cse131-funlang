use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicU64, Ordering};
use dynasmrt::AssemblyOffset;
use crate::types::{TypeInfo, Defn, is_number_tag, is_bool_tag, TypeEnv, Instr};
use crate::typechecker::*;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use crate::optimizer::*;
use crate::compiler::{compile_expr_define_env, CompileCtx, SharedCompileCtx};
use crate::assembly::{instr_to_asm};
use std::rc::Rc;
use std::cell::RefCell;

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
    use std::collections::HashMap as StdHashMap;


    let mut map = registry().lock().unwrap();

    if !map.contains_key(&id) {
        return 0;
    }

    let defns: Vec<Defn> = map.values().map(|info| info.defn.clone()).collect();
    let info = map.get_mut(&id).unwrap();
    let call_count = info.call_count;

    /* ===================================================================
       FIRST CALL — SPECIALIZATION + TYPECHECK + OPTIMIZATION + JIT BUILD
    =================================================================== */
    if call_count == 0 {

        // --- 1. Read args ------------------------------------------------
        let args = unsafe { std::slice::from_raw_parts(args_ptr, count as usize) };

        // --- 2. Infer types only for guards ------------------------------
        let mut inferred_types = Vec::new();
        for &val in args {
            inferred_types.push(
                if is_number_tag(val) { TypeInfo::Num }
                else if is_bool_tag(val) { TypeInfo::Bool }
                else { TypeInfo::Any }
            );
        }

        if info.defn.params.len() != inferred_types.len() {
            info.state = CompileState::OnlySlow;
            info.call_count = 1;
            return 0;
        }

        // --- 3. Build TypeEnv from ORIGINAL annotations ------------------
        let mut var_map = StdHashMap::new();
        let mut fun_arg_types = Vec::new();
        for param in &info.defn.params {
            var_map.insert(param.name.clone(), param.ann_type.clone());
            fun_arg_types.push(param.ann_type.clone());
        }

        let mut tenv = TypeEnv::new();
        tenv.funs
            .insert(info.defn.name.clone(), (fun_arg_types, info.defn.return_type.clone()));

        let local_env = TypeEnv {
            vars: var_map,
            funs: tenv.funs.clone(),
            input_type: TypeInfo::Any,
        };

        // --- 4. Annotate, typecheck, optimize ----------------------------

        let typed_body = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            annotate_expr(&info.defn.body, &local_env, &mut StdHashMap::new())
        }));

        let typed_body = match typed_body {
            Ok(tb) => tb,
            Err(_) => {
                info.state = CompileState::OnlySlow;
                info.call_count = 1;
                return 0;
            }
        };

        let optimized_body = optimize(&typed_body, im::HashMap::new());

        // --- 5. Return type check ----------------------------------------
        let actual = optimized_body.get_type_info();
        if !actual.is_subtype_of(&info.defn.return_type) {
            info.state = CompileState::OnlySlow;
            info.call_count = 1;
            return 0;
        }

        // --- 6. Begin JIT code emission ----------------------------------
        info.recorded_args = Some(inferred_types.clone());
        info.state = CompileState::FastAvailable;

        let mut ops_guard = get_global_ops().lock().unwrap();
        let mut ops = std::mem::replace(
            &mut *ops_guard,
            dynasmrt::x64::Assembler::new().unwrap(),
        );

        let slow_addr_i64 = slow_path_addr as i64;
        let slow_lbl = ops.new_dynamic_label();
        let print_lbl = ops.new_dynamic_label();
        let entry_label = ops.new_dynamic_label();
        let print_fun_addr = crate::common::print_fun as *const () as i64;

        dynasm!(ops
            ; .arch x64
            ; =>entry_label
            ; push rbp
            ; mov rbp, rsp
        );

        // --- Emit guards --------------------------------------------------
        for (i, arg_type) in inferred_types.iter().enumerate() {
            let offset = (16 + i * 8) as i32;

            dynasm!(ops
                ; mov rax, [rbp + offset]
                ; test rax, 1
            );

            match arg_type {
                TypeInfo::Num => dynasm!(ops ; jnz =>slow_lbl),
                TypeInfo::Bool => dynasm!(ops ; jz  =>slow_lbl),
                _ => {}
            }
        }

        // --- Lower optimized body ----------------------------------------
        let shared_ctx = Rc::new(RefCell::new(SharedCompileCtx {
            label_counter: 0,
            max_depth: 0,
            max_outgoing_args: 0,
            define_env: im::HashMap::new(),
        }));

        let mut env = im::HashMap::new();
        for (i, param) in info.defn.params.iter().enumerate() {
            env.insert(param.name.clone(), -(16 + (i * 8) as i32));
        }

        let ctx = CompileCtx {
            loop_depth: 0,
            current_loop_id: -1,
            shared_ctx: shared_ctx.clone(),
            defns: Rc::new(defns),
            env,
        };

        let mut labels = im::HashMap::new();
        labels.insert(info.defn.name.clone(), entry_label);
        labels.insert("type_mismatch_error".to_string(), slow_lbl);
        labels.insert("overflow_error".to_string(), slow_lbl);
        labels.insert("type_error_arithmetic".to_string(), slow_lbl);
        labels.insert("bad_cast_error".to_string(), slow_lbl);
        labels.insert("print_fun_external".to_string(), print_lbl);

        let instrs = compile_expr_define_env(&optimized_body, 8, ctx);


        for instr in &instrs {
            match instr {
                Instr::Label(name)
                | Instr::Call(name)
                | Instr::Jump(_, name)
                | Instr::LeaLabel(_, name) => {
                    if !labels.contains_key(name) {
                        labels.insert(name.clone(), ops.new_dynamic_label());
                    }
                }
                _ => {}
            }
        }

        // compute stack frame size
        let (max_depth, max_outgoing) = {
            let shared = shared_ctx.borrow();
            (shared.max_depth, shared.max_outgoing_args)
        };

        let total_size = max_depth + (max_outgoing * 8);
        let frame_size = ((total_size + 15) / 16) * 16;

        if frame_size > 0 {
            dynasm!(ops ; sub rsp, frame_size);
        }

        // emit instructions
        for instr in &instrs {
            instr_to_asm(instr, &mut ops, &labels);
        }

        dynasm!(ops
            ; mov rsp, rbp
            ; pop rbp
            ; ret

            ; =>slow_lbl
            ; mov rax, QWORD slow_addr_i64
            ; jmp rax

            ; =>print_lbl
            ; mov rax, QWORD print_fun_addr
            ; sub rsp, 8
            ; call rax
            ; add rsp, 8
            ; ret
        );

        // finalize
        match ops.finalize() {
            Ok(buf) => {
                let fast_ptr = buf.ptr(AssemblyOffset(0)) as u64;
                info.fast_addr = Some(fast_ptr);
                std::mem::forget(buf);
            }
            Err(_e) => {
                info.state = CompileState::OnlySlow;
            }
        }

        info.call_count = 1;
        return 0;
    }

    /* ===================================================================
       SUBSEQUENT CALLS — USE FAST VERSION IF AVAILABLE
    =================================================================== */
    info.call_count += 1;

    match info.state {
        CompileState::OnlySlow => {
            0
        }
        CompileState::FastAvailable => {
            let addr = info.fast_addr.unwrap_or(0);
            addr
        }
        CompileState::NotCompiled => unreachable!(),
    }
}
