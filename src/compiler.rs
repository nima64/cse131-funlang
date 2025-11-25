use adder::RuntimeErr;
use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::HashMap;
use std::cell::RefCell;
use std::mem;
use std::panic;
use std::rc::Rc;

use crate::assembly::*;
use crate::common::*;
use crate::types::*;
use crate::optimizer::*;
#[derive(Clone)]
struct CompileCtx {
    loop_depth: i32,
    current_loop_id: i32,
    shared_ctx: Rc<RefCell<SharedCompileCtx>>,
    defns: Rc<Vec<Defn>>,
    env: HashMap<String, i32>
}

struct SharedCompileCtx{
    label_counter: i32,
    max_depth: i32,
    define_env: HashMap<String, Box<i64>>
}

fn check(instrs: &mut Vec<Instr>, condition: Condition) {
    instrs.push(Instr::Mov(Reg::Rcx, TRUE_TAGGED));
    instrs.push(Instr::Mov(Reg::Rax, FALSE_TAGGED));
    instrs.push(Instr::CMov(condition, Reg::Rax, Reg::Rcx));
}

fn err_if(instrs: &mut Vec<Instr>, condition: Condition, err: RuntimeErr) {
    instrs.push(Instr::Jump(condition, err.to_string()));
}

fn compile_expr_define_env(
    e: &ExprT,
    stack_depth: i32,
    ctx: CompileCtx, // Pass by value (copied each call)
) -> Vec<Instr> {
    // Track max depth
    {
        let mut shared = ctx.shared_ctx.borrow_mut();
        if stack_depth > shared.max_depth {
            shared.max_depth = stack_depth;
        }
    }
    match e {
        ExprT::Number(n, _) => vec![Instr::Mov(Reg::Rax, tag_number(*n))],
        ExprT::Boolean(b, _) => vec![Instr::Mov(Reg::Rax, if *b {TRUE_TAGGED} else {FALSE_TAGGED})],
        ExprT::Id(name, _) => {
            // Check env (stack) first for local variables
            if let Some(offset) = ctx.env.get(name) {
                vec![Instr::MovFromStack(Reg::Rax, *offset)]
            // If not in env, check repl_env for defined variables
            } else if let Some(boxed_value) = ctx.shared_ctx.borrow().define_env.get(name) {
                println!("value held in {}: {}", name, untag_number(**boxed_value));
                let addr = boxed_value.as_ref() as *const i64 as i64;
                vec![Instr::Mov(Reg::Rax, addr),
                    Instr::MovDeref(Reg::Rax, Reg::Rax)
                ]
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        }
        ExprT::UnOp(op, subexpr, _) => {
            let mut instrs =
                compile_expr_define_env(subexpr, stack_depth, ctx.clone());
            match op {
                Op1::IsNum | Op1::IsBool => {
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    check(&mut instrs, if let Op1::IsNum = op {Condition::Equal} else {Condition::NotEqual});
                }
                Op1::Add1 | Op1::Sub1 => {
                    instrs.push(Instr::Test(Reg::Rax, 1));// AND with 1 to check LSB and see if its 1 aka BOOL 
                    err_if(&mut instrs, Condition::NotEqual, RuntimeErr::ArithmeticType); // jump to error if it is boolean 
                    instrs.push(if let Op1::Add1 = op {Instr::Add(Reg::Rax, 2)} else {Instr::Sub(Reg::Rax, 2)});
                    err_if(&mut instrs, Condition::Overflow, RuntimeErr::Overflow);
                }
            }
            instrs
        }
        ExprT::BinOp(op, e1, e2, _) => {
            let mut instrs =
                compile_expr_define_env(e1, stack_depth, ctx.clone());
            instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));
            // e2 is on rax and e1 is on the stack
            instrs.extend(compile_expr_define_env(
                e2,
                stack_depth + 8,
                ctx.clone(),
            ));
            instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth)); 
            match op {
                Op2::Equal => {
                    // Type check: both must be same type
                    instrs.push(Instr::MovReg(Reg::Rdx, Reg::Rcx)); 
                    instrs.push(Instr::XorReg(Reg::Rdx, Reg::Rax)); 
                    instrs.push(Instr::Test(Reg::Rdx, 1)); 
                    // If bit 0 is set, types differ -> error
                    err_if(&mut instrs, Condition::NotEqual, RuntimeErr::TypeMismatch);

                    // Types match, do comparison
                    instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));
                    check(&mut instrs, Condition::Equal);
                }
                _ => {
                    // type check 
                    instrs.push(Instr::Test(Reg::Rcx, 1));// AND with 1 to check LSB and see if its 1 aka BOOL 
                    err_if(&mut instrs, Condition::NotEqual, RuntimeErr::ArithmeticType);
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    err_if(&mut instrs, Condition::NotEqual, RuntimeErr::ArithmeticType); 
                    // end type check 

                    match op {
                        Op2::Plus => {
                            instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
                            err_if(&mut instrs, Condition::Overflow, RuntimeErr::Overflow);
                        },
                        Op2::Minus => {
                            instrs.push(Instr::MinusReg(Reg::Rcx, Reg::Rax));
                            instrs.push(Instr::MovReg(Reg::Rax, Reg::Rcx));
                            err_if(&mut instrs, Condition::Overflow, RuntimeErr::Overflow);
                        },
                        Op2::Times => {
                            instrs.push(Instr::Sar(Reg::Rax, 1));
                            instrs.push(Instr::IMul(Reg::Rax, Reg::Rcx));
                            err_if(&mut instrs, Condition::Overflow, RuntimeErr::Overflow);
                        }
                        _ => {
                            instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));
                            let cond = match op {
                                Op2::Less => Condition::Less,
                                Op2::LessEqual => Condition::LessEqual,
                                Op2::Greater => Condition::Greater,
                                Op2::GreaterEqual => Condition::GreaterEqual,
                                _ => unreachable!()
                            };
                            check(&mut instrs, cond);
                        }
                    }

                }
                _ => panic!("Invalid op {:?}!", op),
            }
            instrs
        }
        ExprT::Let(bindings, body, _) => {
            let mut instrs = Vec::new();
            let mut new_env = ctx.env.clone();
            let mut current_depth = stack_depth;
            let mut duplicate_binding = HashMap::new();
            let mut current_ctx: CompileCtx = ctx.clone();

            for (var, val_expr) in bindings {
                if duplicate_binding.contains_key(var) {
                    panic!("Duplicate binding");
                }
                duplicate_binding.insert(var, 1);
                instrs.extend(compile_expr_define_env(
                    val_expr,
                    current_depth,
                    current_ctx.clone(),
                ));
                instrs.push(Instr::MovToStack(Reg::Rax, current_depth));

                new_env.insert(var.clone(), current_depth);
                current_depth += 8;
            }

            // Create new context with updated env for body
            current_ctx.env = new_env;
            instrs.extend(compile_expr_define_env(
                body,
                current_depth,
                current_ctx,
            ));
            instrs
        }
        ExprT::Define(name, e, _) => {
            let instrs = compile_expr_define_env(e, stack_depth, ctx.clone());
            let val = jit_code(&instrs);

            let boxed_val = Box::new(val);
            let mut shared = ctx.shared_ctx.borrow_mut();
            if !shared.define_env.contains_key(name) {
                shared.define_env.insert(name.clone(), boxed_val);
            } else {
                println!("Duplicate binding");
            }

            vec![]
        }
        
        ExprT::Block(exprs, _) => {
            let mut instrs: Vec<Instr> = vec![];

            for expr in exprs {
                instrs.extend(compile_expr_define_env(
                    expr,
                    stack_depth,
                    ctx.clone(),
                ));
            }
            instrs
        }
        ExprT::Loop(e, _) => {
            let loop_id = {
                let mut shared = ctx.shared_ctx.borrow_mut();
                let id = shared.label_counter;
                shared.label_counter += 1;
                id
            };

            let loop_label = format!("loop_{}", loop_id);
            let end_loop_label = format!("endloop_{}", loop_id);

            // Create new context with this loop's ID
            let mut loop_ctx = ctx;
            loop_ctx.current_loop_id = loop_id;
            loop_ctx.loop_depth += 1;

            let mut instrs = vec![];
            instrs.push(Instr::Label(loop_label.clone()));
            instrs.extend(compile_expr_define_env(
                e,
                stack_depth,
                loop_ctx.clone(),
            ));
            instrs.push(Instr::Jump(Condition::Uncond, loop_label.clone()));
            instrs.push(Instr::Label(end_loop_label));
            instrs
        }
        ExprT::Break(e, _) => {
            if ctx.loop_depth == 0 {
                panic!("Invalid: break outside of loop");
            }
            let mut instrs = vec![];
            instrs.extend(compile_expr_define_env(
                e,
                stack_depth,
                ctx.clone(),
            ));
            let loop_end_label = format!("endloop_{}", ctx.current_loop_id);
            instrs.push(Instr::Jump(Condition::Uncond, loop_end_label));
            instrs
        }
        ExprT::Set(name, e, _) => {
            {
                let shared = ctx.shared_ctx.borrow();
                if !ctx.env.contains_key(name) && !shared.define_env.contains_key(name) {
                    panic!("Unbound variable identifier {}", name);
                }
            }

            let mut instrs = vec![];

            instrs.extend(compile_expr_define_env(
                e,
                stack_depth,
                ctx.clone(),
            ));

            if let Some(offset) = ctx.env.get(name) {
                instrs.push(Instr::MovToStack(Reg::Rax, *offset));
            } else {
                let shared = ctx.shared_ctx.borrow();
                if shared.define_env.contains_key(name) {
                    // Get the pointer to the heap-allocated i64
                    let ptr_addr = &**shared.define_env.get(name).unwrap() as *const i64 as i64;
                    instrs.push(Instr::Mov(Reg::Rcx, ptr_addr));
                    instrs.push(Instr::MovToMem(Reg::Rcx, Reg::Rax));
                }
            }

            instrs
        }
        ExprT::If(cond, then_expr, else_expr, _) => {
            let mut instrs = vec![];
            let label_id = {
                let mut shared = ctx.shared_ctx.borrow_mut();
                let id = shared.label_counter;
                shared.label_counter += 1;
                id
            };

            let else_label = format!("else_{}", label_id);
            let end_label = format!("end_if_{}", label_id);

            instrs.extend(compile_expr_define_env(
                cond,
                stack_depth,
                ctx.clone(),
            ));

            instrs.push(Instr::CmpImm(Reg::Rax, FALSE_TAGGED));

            // 3. Jump to else branch if condition equals false
            instrs.push(Instr::Jump(Condition::Equal, else_label.clone()));

            instrs.extend(compile_expr_define_env(
                then_expr,
                stack_depth,
                ctx.clone(),
            ));

            // 5. Jump to end (skip else branch)
            instrs.push(Instr::Jump(Condition::Uncond, end_label.clone()));

            // 6. Else branch label
            instrs.push(Instr::Label(else_label));

            instrs.extend(compile_expr_define_env(
                else_expr,
                stack_depth,
                ctx.clone(),
            ));

            instrs.push(Instr::Label(end_label));

            instrs
        }
        ExprT::FunCall(name, args, _) => {
            // Find the function definition
            let defn = ctx.defns.iter().find(|d| &d.name == name);

            if defn.is_none() {
                panic!("Undefined function: {}", name);
            }

            let defn = defn.unwrap();

            // Check argument count matches
            if args.len() != defn.params.len() {
                panic!(
                    "Function {} expects {} arguments, got {}",
                    name,
                    defn.params.len(),
                    args.len()
                );
            }

            let mut instrs = Vec::new();

            for arg in args {

                instrs.extend(compile_expr_define_env(
                    arg, stack_depth,
                    ctx.clone(),
                ));
                instrs.push(Instr::Push(Reg::Rax));
            }

            instrs.push(Instr::Call(name.clone()));
            if !args.is_empty() {
                instrs.push(Instr::Add(Reg::Rsp, (args.len() * 8) as i32));
            }

            // Result is now in RAX
            instrs
        }
        ExprT::Print(e, _) => {
            let mut instrs = compile_expr_define_env(e, stack_depth, ctx.clone());
            instrs.push(Instr::MovReg(Reg::Rdi, Reg::Rax));
            instrs.push(Instr::Call("print_fun_external".to_string()));
            instrs
        }
        ExprT::Cast(target_type, e, _) => {
            let mut instrs = compile_expr_define_env(e, stack_depth, ctx.clone());

            match target_type {
                TypeInfo::Num => {
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    err_if(&mut instrs, Condition::NotEqual, RuntimeErr::BadCast);
                }
                TypeInfo::Bool => {
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    err_if(&mut instrs, Condition::Equal, RuntimeErr::BadCast);
                }
                TypeInfo::Nothing => {
                    err_if(&mut instrs, Condition::Uncond, RuntimeErr::BadCast);
                }
                TypeInfo::Any => {
                    // No check needed, any value is valid
                }
            }

            instrs
        }
    }
}

pub fn compile_prog(prog: &Prog, define_env: &mut HashMap<String, Box<i64>>) -> Vec<Instr> {
    use crate::typechecker::type_check;

    let base_input_slot = 16;
    let mut env = HashMap::new();
    env.insert("input".to_string(), base_input_slot);

    let shared_ctx = Rc::new(RefCell::new(SharedCompileCtx {
        label_counter: 0,
        max_depth: base_input_slot,
        define_env: std::mem::take(define_env),
    }));

    let ctx = CompileCtx {
        loop_depth: 0,
        current_loop_id: -1,
        shared_ctx: shared_ctx.clone(),
        defns: Rc::new(prog.defns.clone()),
        env: env.clone(),
    };

    let mut instrs = Vec::new();

    instrs.push(Instr::Jump(Condition::Uncond, "main_start".to_string()));

    for defn in &prog.defns {
        instrs.push(Instr::Label(defn.name.clone()));
        instrs.extend(compile_defn(defn, ctx.clone()));
        instrs.push(Instr::Ret);
    }

    instrs.push(Instr::Label("main_start".to_string()));

    let mut env_t = HashMap::new();
    env_t.insert("input".to_string(), Box::new(TypeInfo::Any));
    let main_t = type_check(&prog.main, &env_t);            // TODO: ASK ABOUT TYPE CHECKING
    let optimized_main = optimize(&main_t);
    let body_instrs = compile_expr_define_env(
        &optimized_main,
        base_input_slot + 8,
        ctx.clone(),
    );

    let max_depth = shared_ctx.borrow().max_depth;
    let frame_size: i32 = ((max_depth + 15) / 16) * 16;

    // Prologue
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::MovReg(Reg::Rbp, Reg::Rsp));
    instrs.push(Instr::Sub(Reg::Rsp, frame_size));

    instrs.push(Instr::MovToStack(Reg::Rdi, base_input_slot));
    instrs.extend(body_instrs);

    // Epilogue
    instrs.push(Instr::MovReg(Reg::Rsp, Reg::Rbp));
    instrs.push(Instr::Pop(Reg::Rbp));

    // Move define_env back out
    *define_env = std::mem::take(&mut shared_ctx.borrow_mut().define_env);

    instrs
}

// TODO every function have create a new env?
fn compile_defn(defn: &Defn, mut ctx: CompileCtx) -> Vec<Instr> {
    use crate::typechecker::type_check;

    let mut current_depth = 8;
    let mut max_depth = current_depth;
    let mut env = HashMap::new();

    // assume args are already allocated
    // Arguments are at positive offsets from rbp
    let num_params = defn.params.len();
    for (i, (arg_name, _type)) in defn.params.iter().enumerate() {
        // 8 +  for intial ret addr put on stack by call
        let offset = 8 + ((num_params - i) * 8);

        env.insert(arg_name.clone(), -1 * offset as i32);
    }

    // Update context with function's env
    ctx.env = env;

    // Create type environment for type checking
    let mut env_t = HashMap::new();
    for (param_name, param_type) in &defn.params {
        let type_info = param_type.clone().unwrap_or(TypeInfo::Any);
        env_t.insert(param_name.clone(), Box::new(type_info));
    }
    let body_t = type_check(&defn.body, &env_t);
    let optimized_body = optimize(&body_t);
    let body_instrs = compile_expr_define_env(
        &optimized_body,
        current_depth,
        ctx.clone(),
    );

    let mut instrs = Vec::new();
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::MovReg(Reg::Rbp, Reg::Rsp));
    instrs.push(Instr::Sub(Reg::Rsp, max_depth));

    instrs.extend(body_instrs);

    // Epilogue
    instrs.push(Instr::MovReg(Reg::Rsp, Reg::Rbp));
    instrs.push(Instr::Pop(Reg::Rbp));

    instrs
}

pub fn jit_code_input(instrs: &Vec<Instr>, input: i64) -> i64 {
    let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
        dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    dynasm!(ops; .arch x64);

    let mut labels: HashMap<String, DynamicLabel> = HashMap::new();
    let error_type_mismatch = ops.new_dynamic_label();
    let error_overflow = ops.new_dynamic_label();
    let error_arithmetic = ops.new_dynamic_label();
    let error_bad_cast = ops.new_dynamic_label();
    let error_common = ops.new_dynamic_label();
    let print_fun_external = ops.new_dynamic_label();

    labels.insert("type_mismatch_error".to_string(), error_type_mismatch);
    labels.insert("overflow_error".to_string(), error_overflow);
    labels.insert("type_error_arithmetic".to_string(), error_arithmetic);
    labels.insert("bad_cast_error".to_string(), error_bad_cast);
    labels.insert("print_fun_external".to_string(), print_fun_external);
    let c_func_ptr: extern "C" fn(i64) -> i64 =
        unsafe { mem::transmute(snek_error as *const ()) };
    let print_func_ptr: extern "C" fn(i64) -> i64 =
        unsafe { mem::transmute(print_fun as *const ()) };

    // Pre-create all labels
    for instr in instrs {
        if let Instr::Label(name) = instr {
            labels.insert(name.clone(), ops.new_dynamic_label());
        }
    }

    for instr in instrs {
        instr_to_asm(instr, &mut ops, &labels);
    }

    dynasm!(ops
        ; jmp ->done
        ; =>error_overflow
        ; mov rdi, 2
        ; jmp =>error_common
        ; =>error_type_mismatch
        ; mov rdi, 1
        ; jmp =>error_common
        ; =>error_arithmetic
        ; mov rdi, 3
        ; jmp =>error_common
        ; =>error_bad_cast
        ; mov rdi, 4
        ; =>error_common
        ; mov rax, QWORD c_func_ptr as i64
        ; call rax
        ; ret
        ; =>print_fun_external
        ; sub rsp, 8
        ; mov rax, QWORD print_func_ptr as i64
        ; call rax
        ; add rsp, 8
        ; ret
        ; ->done:
        ; ret
    );

    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn(input);
    result
}

pub fn jit_code(instrs: &Vec<Instr>) -> i64 {
    jit_code_input(instrs, FALSE_TAGGED)
}