use crate::types::*;
use im::HashMap;
use std::collections::HashMap as MutHashMap;
/* 
pub fn union_type(t1: &TypeInfo, t2: &TypeInfo) -> TypeInfo {
    use TypeInfo::*;

    match (t1, t2) {
        (Any, _) | (_, Any) => Any,
        (Num, Num) => Num,
        (Bool, Bool) => Bool,
        (Nothing, Nothing) => Nothing,
        (Nothing, t) => t.clone(),
        (t, Nothing) => t.clone(),
        (Num, Bool) | (Bool, Num) => Any,
    }
}

pub fn type_check_prog(prog: &Prog, env_t: &HashMap<String, Box<TypeInfo>>, define_env_t: &mut MutHashMap<String, Box<TypeInfo>>) -> Result<TypeInfo, String> {
    for defn in &prog.defns {
        type_check_defn(defn, define_env_t, &prog.defns)?;
    }

    let mut loop_ctx = vec![];
    let main_t = type_check_helper(&prog.main, env_t, define_env_t, &prog.defns, &mut loop_ctx);
    Ok(main_t.get_type_info().clone())
}

pub fn type_check_defn(defn: &Defn, define_env_t: &mut MutHashMap<String, Box<TypeInfo>>, defns: &Vec<Defn>) -> Result<(), String> {
    // println!("Type checking function: {}", defn.name);
    // println!("define_env_t has {} entries: {:?}", define_env_t.len(), define_env_t.keys().collect::<Vec<_>>());
    let mut env_t = HashMap::new();
        if defn.return_type.is_none() {
            return Err(format!("Type error: Function {} has typed parameters but no return type", defn.name));
        }

        let expected_return_type = defn.return_type.as_ref().unwrap();

        for (param_name, param_type_opt) in &defn.params {
            let param_type = param_type_opt.clone().unwrap_or(TypeInfo::Any);
            env_t.insert(param_name.clone(), Box::new(param_type));
        }

        let body_t = type_check_helper(&defn.body, &env_t, define_env_t, defns, &mut vec![]);
        let body_type = body_t.get_type_info();

        if !is_subtype(body_type, expected_return_type) {
            return Err(format!(
                "Type error: Function {} body has type {:?}, expected {:?}",
                defn.name, body_type, expected_return_type
            ));
        }
    } else {
        for (param_name, _) in &defn.params {
            env_t.insert(param_name.clone(), Box::new(TypeInfo::Any));
        }

        let _body_t = type_check(&defn.body, &env_t, define_env_t, defns);

    Ok(())
}


pub fn type_check(expr: &ExprT, env_t:&HashMap<String, Box<TypeInfo>>, define_env_t:&mut MutHashMap<String, Box<TypeInfo>>, defns: &Vec<Defn> ) -> ExprT {
    println!("type checker running ");
    let mut loop_ctx = vec![];
    type_check_helper(expr, env_t, define_env_t, defns, &mut loop_ctx ) 
}

pub fn type_check_helper(expr: &ExprT, env_t:&HashMap<String, Box<TypeInfo>>, define_env_t:&mut MutHashMap<String, Box<TypeInfo>>, defns: &Vec<Defn>,  loop_ctx: &mut Vec<Vec<Box<TypeInfo>>> ) -> ExprT {
    
    match expr {
        ExprT::Number(n, _) => ExprT::Number(*n, TypeInfo::Num),
        ExprT::Boolean(b, _) => ExprT::Boolean(*b, TypeInfo::Bool),
        ExprT::Id(name, _) => {
            if let Some(type_info) = env_t.get(name) {
                ExprT::Id(name.clone(), *type_info.clone())
            } else if let Some(type_info) = define_env_t.get(name) {
                ExprT::Id(name.clone(), *type_info.clone())
            } else if let Some(defn) = defns.iter().find(|d| &d.name == name) {
                // If it's a function definition, return its return type or Any
                let func_type = defn.return_type.clone().unwrap_or(TypeInfo::Any);
                ExprT::Id(name.clone(), func_type)
            } else if name == "input" {
                ExprT::Id("input".to_string(), TypeInfo::Any)
            } else {
                panic!("unbound variable {}", name);
            }
        }
        ExprT::Let(bindings, body, _) => {
            let mut t_bindings = vec![];
            let mut new_env = env_t.clone();
            let mut duplicate_binding = HashMap::new();

            for (var, val_expr) in bindings {
                if duplicate_binding.contains_key(var) {
                    panic!("Duplicate binding");
                }
                duplicate_binding.insert(var, 1);

                let val_t = type_check_helper(val_expr, &new_env, define_env_t, defns, loop_ctx);
                let type_info = val_t.get_type_info().clone();

                t_bindings.push((var.clone(), val_t));
                new_env.insert(var.clone(), Box::new(type_info));
            }

            let body_t = type_check_helper(body, &new_env, define_env_t, defns, loop_ctx);
            let body_type = body_t.get_type_info().clone();

            ExprT::Let(t_bindings, Box::new(body_t), body_type)
        }
        ExprT::Define(name, expr, _) => {
            let expr_t = type_check_helper(expr, &env_t, define_env_t, defns, loop_ctx);
            let expr_type = expr_t.get_type_info().clone();
            define_env_t.insert(name.clone(), Box::new(expr_type.clone()));
            ExprT::Define(name.clone(), Box::new(expr_t), expr_type)
        }
        ExprT::Block(exprs, _) => {
            let mut exprs_t = vec![];
            for expr in exprs {
                exprs_t.push(type_check_helper(expr, env_t, define_env_t, defns, loop_ctx));
            }
            let block_type = match exprs_t.last() {
                Some(last_element) => last_element.get_type_info().clone(),
                None => panic!("empty block expression"),
            };
            ExprT::Block(exprs_t, block_type)
        }
        ExprT::UnOp(op, subexpr, _) => match op {
            Op1::Add1 | Op1::Sub1 => {
                let subexpr_t = type_check_helper(subexpr, env_t, define_env_t, defns, loop_ctx);
                if !matches!(subexpr_t.get_type_info(), TypeInfo::Num) {
                    panic!("Expected Num type, got: {:?}", subexpr_t);
                }
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Num)
            }
            Op1::IsNum => {
                let subexpr_t = type_check_helper(subexpr, env_t, define_env_t, defns, loop_ctx);
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Bool)
            }
            Op1::IsBool => {
                let subexpr_t = type_check_helper(subexpr, env_t, define_env_t, defns, loop_ctx);
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Bool)
            }
        },
        ExprT::BinOp(op, left, right, _) => {
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    let left_t = type_check_helper(left, env_t, define_env_t, defns, loop_ctx);
                    if !matches!(left_t.get_type_info(), TypeInfo::Num) {
                        panic!("Expected Num type for left operand, got: {:?}", left_t);
                    }

                    let right_t = type_check_helper(right, env_t, define_env_t, defns, loop_ctx);
                    if !matches!(right_t.get_type_info(), TypeInfo::Num) {
                        panic!("Expected Num type for right operand, got: {:?}", right_t);
                    }
                    ExprT::BinOp(op.clone(), Box::new(left_t), Box::new(right_t), TypeInfo::Num)
                }
                Op2::Less | Op2::Greater | Op2::LessEqual | Op2::GreaterEqual | Op2::Equal => {
                    let left_t = type_check_helper(left, env_t, define_env_t, defns, loop_ctx);
                    let right_t = type_check_helper(right, env_t, define_env_t, defns, loop_ctx);
                    ExprT::BinOp(op.clone(), Box::new(left_t), Box::new(right_t), TypeInfo::Bool)
                }
            }
        }
        ExprT::If(cond, then_expr, else_expr, _) => {
            let cond_t = type_check_helper(cond, env_t, define_env_t, defns, loop_ctx);
            if !matches!(cond_t.get_type_info(), TypeInfo::Bool) {
                panic!("Type error: Expected Bool type for condition, got: {:?}", cond_t.get_type_info());
            }
            let then_t = type_check_helper(then_expr, env_t, define_env_t, defns, loop_ctx);
            let else_t = type_check_helper(else_expr, env_t, define_env_t, defns, loop_ctx);
            let if_type = union_type(then_t.get_type_info(), else_t.get_type_info());
            ExprT::If(Box::new(cond_t), Box::new(then_t), Box::new(else_t), if_type)
        }
        ExprT::Loop(body, _) => {
            loop_ctx.push(vec![]);
            let body_t = type_check_helper(body, env_t, define_env_t, defns, loop_ctx);
            let t_infos = loop_ctx.last().unwrap();

            // println!("Loops : {:?} ", loop_ctx);
            // Keep commented because it our test uses the check during compilation phase
            // if t_infos.len() == 0{
            //     panic!("no breaks found for this loop");
            // }
            let mut t_info: TypeInfo = (**t_infos.first().unwrap()).clone();
            let t_prev = t_info.clone();

            for curr_tinfo in t_infos{
                if **curr_tinfo != t_prev{
                    t_info = TypeInfo::Any;
                    break;
                }
            }
            loop_ctx.pop();
            ExprT::Loop(Box::new(body_t), t_info)
        }

        ExprT::Break(expr, _) => {
            let expr_t: ExprT = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            let t_info = expr_t.get_type_info().clone();
            let curr_loop = loop_ctx.last_mut().expect("break outside of loop");
            curr_loop.push(Box::new(t_info.clone()));
            ExprT::Break(Box::new(expr_t), t_info)
        }
        ExprT::Set(name, expr, _) => {
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            let t_info = expr_t.get_type_info().clone();

            if let Some(type_info) = env_t.get(name) {
                if !is_subtype(expr_t.get_type_info(), &(*type_info)) {
                    panic!("Type error: {:?} is not a subtype of {:?}", expr_t.get_type_info(), *type_info)
                }
            } else if let Some(type_info) = define_env_t.get(name) {
                if !is_subtype(expr_t.get_type_info(), &(*type_info)) {
                    panic!("Type error: {:?} is not a subtype of {:?}", expr_t.get_type_info(), *type_info)
                }
                // Update the type in define_env_t
                define_env_t.insert(name.clone(), Box::new(expr_t.get_type_info().clone()));
            } else {
                panic!("Unbound variable identifier {}", name);
            }

            ExprT::Set(name.clone(), Box::new(expr_t), t_info)
        }
        ExprT::FunCall(name, args, _) => {
            let mut args_t = vec![];
            for arg in args {
                args_t.push(type_check_helper(arg, env_t, define_env_t, defns, loop_ctx));
            }

            // Look up the function definition and get its return type
            let return_type = if let Some(defn) = defns.iter().find(|d| &d.name == name) {
                defn.return_type.clone().unwrap_or(TypeInfo::Any)
            } else {
                TypeInfo::Any
            };

            ExprT::FunCall(name.clone(), args_t, return_type)
        }
        ExprT::Print(expr, _) => {
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            let t_info = expr_t.get_type_info().clone();
            ExprT::Print(Box::new(expr_t), t_info)
        }
        ExprT::Cast(target_type, expr, _) => {
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            ExprT::Cast(target_type.clone(), Box::new(expr_t), target_type.clone())
        }
    }
}
 */
pub fn annotate_program(program: &mut crate::syntax::Program, enable: bool) -> Result<(), String> {
    if !enable {
        return Ok(());
    }

    // build initial function environment from declared arg types and declared ret types
    let mut tenv = TypeEnv::new();
    for def in &program.defs {
        let arg_types = def.args.iter().map(|a| a.ann_type.clone()).collect::<Vec<_>>();
        tenv.funs.insert(def.name.clone(), (arg_types, def.ret_type.clone()));
    }

    // annotate each function body and update its return type based on typecheck
    let mut new_defs: Vec<Definition> = Vec::new();
    for def in &program.defs {
        let mut var_map = HashMap::new();
        for a in def.args.iter() {
            var_map.insert(a.name.clone(), a.ann_type.clone());
        }
        let local_env = TypeEnv { vars: var_map, funs: tenv.funs.clone() };
        let ann_body = annotate_expr(&def.body, &local_env)?;
        let (ret_t, _ret_break) = typecheck(&def.body, &local_env)?;
        let mut new_def = def.clone();
        new_def.body = ann_body;
        new_def.ret_type = ret_t.clone();
        // update function env with inferred return type
        let arg_types = new_def.args.iter().map(|a| a.ann_type.clone()).collect::<Vec<_>>();
        tenv.funs.insert(new_def.name.clone(), (arg_types, ret_t));
        new_defs.push(new_def);
    }

    // annotate main
    let global_env = TypeEnv { vars: HashMap::new(), funs: tenv.funs.clone() };
    let ann_main = annotate_expr(&program.main, &global_env)?;

    // replace program contents
    program.defs = new_defs;
    program.main = ann_main;
    Ok(())
}

 pub fn annotate_expr(e: &Expr, type_env: &TypeEnv) -> Result<crate::syntax::Expr, String> {
    match e {
        Expr::Number(_, n) => Ok(Expr::Number(Type::Num, *n)),
        Expr::Boolean(_, b) => Ok(Expr::Boolean(Type::Bool, *b)),
        Expr::Id(_, name) => {
            let var_type = if name == "input" { Type::Any } else { type_env.lookup_var(name) };
            Ok(Expr::Id(var_type, name.clone()))
        }
        Expr::Loop(_, body) => {
            let annotated_body = annotate_expr(body, type_env)?;
            let (t, _) = typecheck(e, type_env)?;
            Ok(Expr::Loop(t, Box::new(annotated_body)))
        }
        Expr::Break(_, be) => {
            let annotated_be = annotate_expr(be, type_env)?;
            let (_t, _b) = typecheck(e, type_env)?;
            Ok(Expr::Break(Type::Nothing, Box::new(annotated_be)))
        }
        Expr::Block(_, exprs) => {
            let mut annotated = Vec::new();
            for ex in exprs {
                annotated.push(annotate_expr(ex, type_env)?);
            }
            // compute block type via typecheck
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::Block(t, annotated))
        }
        Expr::Call(_, name, args) => {
            let mut ann_args = Vec::new();
            for a in args {
                ann_args.push(annotate_expr(a, type_env)?);
            }
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::Call(t, name.clone(), ann_args))
        }
        Expr::If(_, cond, then_e, else_e) => {
            let ann_cond = annotate_expr(cond, type_env)?;
            let ann_then = annotate_expr(then_e, type_env)?;
            let ann_else = annotate_expr(else_e, type_env)?;
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::If(t, Box::new(ann_cond), Box::new(ann_then), Box::new(ann_else)))
        }
        Expr::Let(_, bindings, body) => {
            // incrementally annotate bindings similar to the typecheck behavior
            let mut new_vars = type_env.vars.clone();
            let mut ann_bindings: Vec<(String, crate::syntax::Expr)> = Vec::new();
            for (name, be) in bindings {
                let env_snapshot = TypeEnv { vars: new_vars.clone(), funs: type_env.funs.clone() };
                let ann_bind = annotate_expr(be, &env_snapshot)?;
                let (bt, _bb) = typecheck(be, &env_snapshot)?;
                new_vars.insert(name.clone(), bt);
                ann_bindings.push((name.clone(), ann_bind));
            }
            let new_env = TypeEnv { vars: new_vars.clone(), funs: type_env.funs.clone() };
            let ann_body = annotate_expr(body, &new_env)?;
            let (body_t, _bb) = typecheck(body, &new_env)?;
            // convert bindings to Vec<(String, Expr)>
            let final_bindings = ann_bindings.into_iter().map(|(n,a)|(n,a)).collect::<Vec<(String, crate::syntax::Expr)>>();
            Ok(Expr::Let(body_t, final_bindings, Box::new(ann_body)))
        }
        Expr::Set(_, name, val) => {
            let ann_val = annotate_expr(val, type_env)?;
            let (e_t, _b) = typecheck(val, type_env)?;
            let var_t = type_env.lookup_var(name);
            if !e_t.is_subtype_of(&var_t) {
                return Err(format!("Type Error: expected {:?} but found {:?} in set expression for variable {}", var_t, e_t, name));
            }
            Ok(Expr::Set(Type::Nothing, name.clone(), Box::new(ann_val)))
        }
        Expr::UnOp(_, op, ex) => {
            let ann_ex = annotate_expr(ex, type_env)?;
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::UnOp(t, op.clone(), Box::new(ann_ex)))
        }
        Expr::BinOp(_, op, e1, e2) => {
            let ann1 = annotate_expr(e1, type_env)?;
            let ann2 = annotate_expr(e2, type_env)?;
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::BinOp(t, op.clone(), Box::new(ann1), Box::new(ann2)))
        }
        Expr::Cast(_, ex) => {
            let ann = annotate_expr(ex, type_env)?;
            let (t, _b) = typecheck(e, type_env)?;
            Ok(Expr::Cast(t, Box::new(ann)))
        }
    }
}

pub fn typecheck(e: &Expr, type_env: &TypeEnv) -> Result<(Type, Type), String> {
    match e {
        Expr::Number(_, n) => {
            Ok((Type::Num, Type::Nothing))
        },
        Expr::Boolean(_, b) => {
            Ok((Type::Bool, Type::Nothing))
        },
        Expr::Id(_, name) => {
            let var_type = if name == "input" { Type::Any } else { type_env.lookup_var(name) };
            Ok((var_type.clone(), Type::Nothing))
        },
        
        Expr::Loop(_, body) => {
            let (_, eb) = typecheck(body, type_env)?;
            Ok((eb, Type::Nothing))
        },
        Expr::Break(_, e) => {
            let (te, eb) = typecheck(e, type_env)?;
            Ok((Type::Nothing, te.union(&eb)))
        },
        Expr::Block(_, exprs) => {
            let mut block_type = Type::Nothing;
            let mut block_break_type = Type::Nothing;
            for expr in exprs {
                let (et, eb) = typecheck(expr, type_env)?;
                block_type = et;
                block_break_type = block_break_type.union(&eb);
            }
            Ok((block_type, block_break_type))
        },

        Expr::Call(_, name, args) => {
            let (param_types, ret_type) = type_env.lookup_fun(name);
            if param_types.len() != args.len() {
                return Err(format!("Type Error: expected {} arguments but found {} in call to function {}", param_types.len(), args.len(), name));
            }
            for (i, arg) in args.iter().enumerate() {
                let (arg_type, _) = typecheck(arg, type_env)?;
                if !arg_type.is_subtype_of(&param_types[i]) {
                    return Err(format!("Type Error: expected {:?} but found {:?} in argument {} of function {}", param_types[i], arg_type, i+1, name));
                }
                // ignore arg_break_type since breaks in arguments are not allowed
            }
            Ok((ret_type, Type::Nothing))
        }

        Expr::If(_, cond, then_e, else_e) => {
            let (cond_type, cond_break_type) = typecheck(cond, type_env)?;
            if !cond_type.is_subtype_of(&Type::Bool) {
                return Err(format!("Type Error: expected Bool but found {:?} in if condition", cond_type));
            }
            let (then_type, then_break_type) = typecheck(then_e, type_env)?;
            let (else_type, else_break_type) = typecheck(else_e, type_env)?;

            /*if !then_type.is_subtype_of(t) {
                return Err(format!("Type Error: expected {:?} but found {:?} in then branch", t, then_type));
            }
            if !else_type.is_subtype_of(t) {
                return Err(format!("Type Error: expected {:?} but found {:?} in else branch", t, else_type));
            }*/
            Ok((then_type.union(&else_type), cond_break_type.union(&then_break_type).union(&else_break_type)))
        },

        Expr::Let(t, bindings, body) => {
            let mut new_env = type_env.vars.clone();
            let mut bindings_break_type = Type::Nothing;
            for (name, bind) in bindings {
                let (bind_type, bind_break_type) = typecheck(bind, &TypeEnv {vars: new_env.clone(), funs: type_env.funs.clone()})?;
                new_env.insert(name.clone(), bind_type);
                bindings_break_type = bindings_break_type.union(&bind_break_type);
            }
            let new_type_env = TypeEnv {vars: new_env, funs: type_env.funs.clone()};
            let (body_type, body_break_type) = typecheck(body, &new_type_env)?;
            /*if !body_type.is_subtype_of(t) {
                return Err(format!("Type Error: expected {:?} but found {:?} in let body", t, body_type));
            }*/
            Ok((body_type, bindings_break_type.union(&body_break_type)))
        },

        Expr::Set(t, name, e) => {
            let (e_type, e_break_type) = typecheck(e, type_env)?;
            let var_type = type_env.lookup_var(name);
            if !e_type.is_subtype_of(&var_type) {
                return Err(format!("Type Error: expected {:?} but found {:?} in set expression for variable {}", var_type, e_type, name));
            }
            Ok((Type::Nothing, e_break_type))
        },

        Expr::UnOp(t, op, e) => {
            let (et, eb) = typecheck(e, type_env)?;
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    if !et.is_subtype_of(&Type::Num) {
                        return Err(format!("Type Error: expected Num but found {:?} in operand of {:?}", et, op));
                    }
                    Ok((Type::Num, eb))
                },
                Op1::IsNum => Ok((Type::Bool, eb)),
                Op1::IsBool => Ok((Type::Bool, eb)),
                Op1::Print => Ok((et, eb)),
            }
        },

        Expr::BinOp(t, op, e1, e2) => {
            let (t1, b1) = typecheck(e1, type_env)?;
            let (t2, b2) = typecheck(e2, type_env)?;

            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    if !t1.is_subtype_of(&Type::Num) {
                        return Err(format!("Type Error: expected Num but found {:?} in left operand of {:?}", t1, op));
                    }
                    if !t2.is_subtype_of(&Type::Num) {
                        return Err(format!("Type Error: expected Num but found {:?} in right operand of {:?}", t2, op));
                    }
                    Ok((Type::Num, b1.union(&b2)))
                },
                Op2::Less | Op2::LessEqual | Op2::Greater | Op2::GreaterEqual => {
                    if !t1.is_subtype_of(&Type::Num) {
                        return Err(format!("Type Error: expected Num but found {:?} in left operand of {:?}", t1, op));
                    }
                    if !t2.is_subtype_of(&Type::Num) {
                        return Err(format!("Type Error: expected Num but found {:?} in right operand of {:?}", t2, op));
                    }
                    Ok((Type::Bool, b1.union(&b2)))
                },
                Op2::Equal => {
                    if !(t1.is_subtype_of(&Type::Num) && t2.is_subtype_of(&Type::Num)) &&
                       !(t1.is_subtype_of(&Type::Bool) && t2.is_subtype_of(&Type::Bool)) {
                        return Err(format!("Type Error: expected both operands of {:?} to be Num or Bool but found {:?} and {:?}", op, t1, t2));
                    }
                    Ok((Type::Bool, b1.union(&b2)))
                }
            }
        },
        Expr::Cast(t, e) => {
            let (et, eb) = typecheck(e, type_env)?;
            if !t.is_subtype_of(&et) {
                return Err(format!("Type Error: cannot cast {:?} to {:?}", et, t));
            }
            Ok((t.clone(), eb))
        },
    }
}