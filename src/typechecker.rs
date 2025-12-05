use crate::types::*;
use std::collections::HashMap;
use crate::types::ExprT::*;
use std::panic::{catch_unwind, AssertUnwindSafe};

pub fn annotate_expr(e: &ExprT, type_env: &TypeEnv, global_var_env: &mut HashMap<String, TypeInfo>) -> ExprT {
    match e {
        Number(n, _) => Number(*n, TypeInfo::Num),
        Boolean(b, _) => Boolean(*b, TypeInfo::Bool),
        Id(name, _) => {
            let var_type = if name == "input" { type_env.input_type.clone() } else { type_env.lookup_var(name) };
            Id(name.clone(), var_type)
        }
        Loop(body, _) => {
            let annotated_body = annotate_expr(body, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            Loop(Box::new(annotated_body), t)
        }
        Break(be, _) => {
            let annotated_be = annotate_expr(be, type_env, global_var_env);
            let (_t, _) = typecheck(e, type_env);
            Break(Box::new(annotated_be), TypeInfo::Nothing)
        }
        Block(exprs, _) => {
            let mut annotated = Vec::new();
            for ex in exprs {
                annotated.push(annotate_expr(ex, type_env, global_var_env));
            }
            // compute block type via typecheck
            let (t, _) = typecheck(e, type_env);
            Block(annotated, t)
        }
        FunCall(name, args, _) => {
            let mut ann_args = Vec::new();
            for a in args {
                ann_args.push(annotate_expr(a, type_env, global_var_env));
            }
            let (t, _) = typecheck(e, type_env);
            FunCall(name.clone(), ann_args, t)
        }
        If(cond, then_e, else_e, _) => {
            let ann_cond = annotate_expr(cond, type_env, global_var_env);
            let ann_then = annotate_expr(then_e, type_env, global_var_env);
            let ann_else = annotate_expr(else_e, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            If(Box::new(ann_cond), Box::new(ann_then), Box::new(ann_else), t)
        }
        Let(bindings, body, _) => {
            // incrementally annotate bindings similar to the typecheck behavior
            let mut new_vars = type_env.vars.clone();
            let mut ann_bindings: Vec<(String, ExprT)> = Vec::new();
            for (name, be) in bindings {
                    let env_snapshot = TypeEnv { vars: new_vars.clone(), funs: type_env.funs.clone(), input_type: type_env.input_type.clone() };
                let ann_bind = annotate_expr(be, &env_snapshot, global_var_env);
                let (bt, _) = typecheck(be, &env_snapshot);
                new_vars.insert(name.clone(), bt);
                ann_bindings.push((name.clone(), ann_bind));
            }
            let new_env = TypeEnv { vars: new_vars.clone(), funs: type_env.funs.clone(), input_type: type_env.input_type.clone() };
            let ann_body = annotate_expr(body, &new_env, global_var_env);
            let (body_t, _) = typecheck(body, &new_env);
            // convert bindings to Vec<(String, ExprT)>
            let final_bindings = ann_bindings.into_iter().map(|(n,a)|(n,a)).collect::<Vec<(String, ExprT)>>();
            Let(final_bindings, Box::new(ann_body), body_t)
        }
        Set(name, val, _) => {
            //println!("{:?}", type_env.vars);
            let ann_val = annotate_expr(val, type_env, global_var_env);
            let (e_t, _) = typecheck(val, type_env);
            let var_t = type_env.lookup_var(name);
            if !e_t.is_subtype_of(&var_t) {
                panic!("Type Error: expected {:?} but found {:?} in set expression for variable {}", var_t, e_t, name);
            }
            Set(name.clone(), Box::new(ann_val), e_t)
        }
        UnOp(op, ex, _) => {
            let ann_ex = annotate_expr(ex, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            UnOp(op.clone(), Box::new(ann_ex), t)
        }
        Print(ex, _) => {
            let ann = annotate_expr(ex, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            Print(Box::new(ann), t)
        }
        BinOp(op, e1, e2, _) => {
            let ann1 = annotate_expr(e1, type_env, global_var_env);
            let ann2 = annotate_expr(e2, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            BinOp(op.clone(), Box::new(ann1), Box::new(ann2), t)
        }
        Cast(target_type, ex, _) => {
            let ann = annotate_expr(ex, type_env, global_var_env);
            let (t, _) = typecheck(e, type_env);
            Cast(target_type.clone(), Box::new(ann), t)
        }
        Define(name, ex, _) => {
            let ann = annotate_expr(ex, type_env, global_var_env);
            let (t, _) = typecheck(ex, type_env);
            // Add to global_var_env
            global_var_env.insert(name.clone(), t.clone());
            Define(name.clone(), Box::new(ann), t)
        }
    }
}

// Annotate a Program (produce and replace with annotated expressions)
// This function mutates `program` in-place to replace bodies and main with annotated copies.
pub fn annotate_program(program: &mut Prog, global_var_env: &mut HashMap<String, TypeInfo>, input_type: TypeInfo) -> Result<(), String> {
    // build initial function environment from declared arg types and declared ret types
    let mut tenv = TypeEnv::new();
    for def in &program.defns {
        let arg_types = def.params.iter().map(|a| a.ann_type.clone()).collect::<Vec<_>>();
        tenv.funs.insert(def.name.clone(), (arg_types, def.return_type.clone()));
    }

    // annotate each function body and update its return type based on typecheck
    let mut new_defs: Vec<Defn> = Vec::new();
    for def in &program.defns {
        // Work on a temporary clone of the global var env so that annotation
        // panics don't leave partial updates. On success we commit tmp_global
        // back into the real `global_var_env`.
        let mut tmp_global = global_var_env.clone();

        // Build var map based on tmp_global and the function parameters
        let mut var_map = tmp_global.clone();
        for a in def.params.iter() {
            var_map.insert(a.name.clone(), a.ann_type.clone());
        }
    let local_env = TypeEnv { vars: var_map, funs: tenv.funs.clone(), input_type: input_type.clone() };

        let res = catch_unwind(AssertUnwindSafe(|| annotate_expr(&def.body, &local_env, &mut tmp_global)));
        let ann_body = match res {
            Ok(ann) => {
                // commit any global defines produced during annotation
                *global_var_env = tmp_global.clone();
                ann
            }
            Err(payload) => {
                let msg = if let Some(s) = payload.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = payload.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "annotate_expr panicked with non-string payload".to_string()
                };
                return Err(msg);
            }
        };

        let (ret_t, _ret_break) = typecheck(&def.body, &local_env);
        let mut new_def = def.clone();
        new_def.body = Box::new(ann_body);
        new_def.return_type = ret_t.clone();
        // update function env with inferred return type
        let arg_types = new_def.params.iter().map(|a| a.ann_type.clone()).collect::<Vec<_>>();
        tenv.funs.insert(new_def.name.clone(), (arg_types, ret_t));
        new_defs.push(new_def);
    }

    // annotate main using a temporary clone of the global var env and commit on success
    let global_env = TypeEnv { vars: global_var_env.clone(), funs: tenv.funs.clone(), input_type: input_type.clone() };
    let mut tmp_global = global_var_env.clone();
    let res_main = catch_unwind(AssertUnwindSafe(|| annotate_expr(&program.main, &global_env, &mut tmp_global)));
    let ann_main = match res_main {
        Ok(ann) => {
            *global_var_env = tmp_global;
            ann
        }
        Err(payload) => {
            let msg = if let Some(s) = payload.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = payload.downcast_ref::<String>() {
                s.clone()
            } else {
                "annotate_expr panicked with non-string payload".to_string()
            };
            return Err(msg);
        }
    };

    // replace program contents
    program.defns = new_defs;
    program.main = Box::new(ann_main);
    Ok(())
}

// Annotate expressions in-place with their computed return types.
// Returns (expr_type, break_type) pair; mutates expr to update type annotation.
pub fn typecheck(e: &ExprT, type_env: &TypeEnv) -> (TypeInfo, TypeInfo) {
    match e {
        Number(_n, _) => (TypeInfo::Num, TypeInfo::Nothing),
        Boolean(_b, _) => (TypeInfo::Bool, TypeInfo::Nothing),
        Id(name, _) => (
            if name == "input" { type_env.input_type.clone() } else { type_env.lookup_var(name) },
            TypeInfo::Nothing
        ),
        Loop(body, _) => {
            let (_, eb) = typecheck(body, type_env);
            (eb, TypeInfo::Nothing)
        },
        Break(e, _) => {
            let (te, eb) = typecheck(e, type_env);
            (TypeInfo::Nothing, te.union(&eb))
        },
        Block(exprs, _) => {
            let mut block_type = TypeInfo::Nothing;
            let mut block_break_type = TypeInfo::Nothing;
            for expr in exprs {
                let (et, eb) = typecheck(expr, type_env);
                block_type = et;
                block_break_type = block_break_type.union(&eb);
            }
            (block_type, block_break_type)
        },

        FunCall(name, args, _) => {
            let (param_types, ret_type) = type_env.lookup_fun(name);
            if param_types.len() != args.len() {
                panic!("Type Error: expected {} arguments but found {} in call to function {}", param_types.len(), args.len(), name);
            }
            for (i, arg) in args.iter().enumerate() {
                let (arg_type, _) = typecheck(arg, type_env);
                if !arg_type.is_subtype_of(&param_types[i]) {
                    panic!("Type Error: expected {:?} but found {:?} in argument {} of function {}", param_types[i], arg_type, i+1, name);
                }
                // ignore arg_break_type since breaks in arguments are not allowed
            }
            (ret_type, TypeInfo::Nothing)
        }

        If(cond, then_e, else_e, _) => {
            let (cond_type, cond_break_type) = typecheck(cond, type_env);
            if !cond_type.is_subtype_of(&TypeInfo::Bool) {
                panic!("Type Error: expected Bool but found {:?} in if condition", cond_type);
            }
            let (then_type, then_break_type) = typecheck(then_e, type_env);
            let (else_type, else_break_type) = typecheck(else_e, type_env);

            (then_type.union(&else_type), cond_break_type.union(&then_break_type).union(&else_break_type))
        },

        Let(bindings, body, _t) => {
            let mut new_env = type_env.vars.clone();
            let mut bindings_break_type = TypeInfo::Nothing;
            for (name, bind) in bindings {
                let (bind_type, bind_break_type) = typecheck(bind, &TypeEnv { vars: new_env.clone(), funs: type_env.funs.clone(), input_type: type_env.input_type.clone()});
                new_env.insert(name.clone(), bind_type);
                bindings_break_type = bindings_break_type.union(&bind_break_type);
            }
            let new_type_env = TypeEnv { vars: new_env, funs: type_env.funs.clone(), input_type: type_env.input_type.clone() };
            let (body_type, body_break_type) = typecheck(body, &new_type_env);
            /*if !body_type.is_subtype_of(t) {
                return Err(format!("Type Error: expected {:?} but found {:?} in let body", t, body_type));
            }*/
            (body_type, bindings_break_type.union(&body_break_type))
        },

        Set(name, e, _t) => {
            let (e_type, e_break_type) = typecheck(e, type_env);
            let var_type = type_env.lookup_var(name);
            if !e_type.is_subtype_of(&var_type) {
                panic!("Type Error: expected {:?} but found {:?} in set expression for variable {}", var_type, e_type, name);
            }
            (TypeInfo::Nothing, e_break_type)
        },

        UnOp(op, e, _t) => {
            let (et, eb) = typecheck(e, type_env);
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    if !et.is_subtype_of(&TypeInfo::Num) {
                        panic!("Type Error: expected Num but found {:?} in operand of {:?}", et, op);
                    }
                    (TypeInfo::Num, eb)
                },
                Op1::IsNum => (TypeInfo::Bool, eb),
                Op1::IsBool => (TypeInfo::Bool, eb),
            }
        },
        Print(e, _) => typecheck(e, type_env),

        BinOp(op, e1, e2, _t) => {
            let (t1, b1) = typecheck(e1, type_env);
            let (t2, b2) = typecheck(e2, type_env);

            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    if !t1.is_subtype_of(&TypeInfo::Num) {
                        panic!("Type Error: expected Num but found {:?} in left operand of {:?}", t1, op);
                    }
                    if !t2.is_subtype_of(&TypeInfo::Num) {
                        panic!("Type Error: expected Num but found {:?} in right operand of {:?}", t2, op);
                    }
                    (TypeInfo::Num, b1.union(&b2))
                },
                Op2::Less | Op2::LessEqual | Op2::Greater | Op2::GreaterEqual => {
                    if !t1.is_subtype_of(&TypeInfo::Num) {
                        panic!("Type Error: expected Num but found {:?} in left operand of {:?}", t1, op);
                    }
                    if !t2.is_subtype_of(&TypeInfo::Num) {
                        panic!("Type Error: expected Num but found {:?} in right operand of {:?}", t2, op);
                    }
                    (TypeInfo::Bool, b1.union(&b2))
                },
                Op2::Equal => {
                    if !(t1.is_subtype_of(&TypeInfo::Num) && t2.is_subtype_of(&TypeInfo::Num)) &&
                       !(t1.is_subtype_of(&TypeInfo::Bool) && t2.is_subtype_of(&TypeInfo::Bool)) {
                        panic!("Type Error: expected both operands of {:?} to be Num or Bool but found {:?} and {:?}", op, t1, t2);
                    }
                    (TypeInfo::Bool, b1.union(&b2))
                }
            }
        },
        Cast(_target_type, e, t) => {
            let (et, eb) = typecheck(e, type_env);
            if !t.is_subtype_of(&et) {
                panic!("Type Error: cannot cast {:?} to {:?}", et, t);
            }
            (t.clone(), eb)
        },
        Define(_name, e, _) => {
            let (_e_t, e_b) = typecheck(e, type_env);
            (TypeInfo::Nothing, e_b)
        }
    }
}