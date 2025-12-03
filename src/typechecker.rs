use crate::types::*;
use im::HashMap;
use std::collections::HashMap as MutHashMap;

pub fn annotate_any(expr: &Expr) -> ExprT {
    match expr {
        Expr::Number(n) => ExprT::Number(*n, TypeInfo::Any),
        Expr::Boolean(b) => ExprT::Boolean(*b, TypeInfo::Any),
        Expr::Id(name) => ExprT::Id(name.clone(), TypeInfo::Any),
        Expr::UnOp(op, subexpr) => {
            ExprT::UnOp(op.clone(), Box::new(annotate_any(subexpr)), TypeInfo::Any)
        }
        Expr::BinOp(op, e1, e2) => {
            ExprT::BinOp(op.clone(), Box::new(annotate_any(e1)), Box::new(annotate_any(e2)), TypeInfo::Any)
        }
        Expr::Let(bindings, body) => {
            let t_bindings = bindings.iter()
                .map(|(name, expr)| (name.clone(), annotate_any(expr)))
                .collect();
            ExprT::Let(t_bindings, Box::new(annotate_any(body)), TypeInfo::Any)
        }
        Expr::Define(name, expr) => {
            ExprT::Define(name.clone(), Box::new(annotate_any(expr)), TypeInfo::Any)
        }
        Expr::Block(exprs) => {
            let exprs_t = exprs.iter().map(|e| annotate_any(e)).collect();
            ExprT::Block(exprs_t, TypeInfo::Any)
        }
        Expr::If(cond, then_expr, else_expr) => {
            ExprT::If(Box::new(annotate_any(cond)), Box::new(annotate_any(then_expr)), Box::new(annotate_any(else_expr)), TypeInfo::Any)
        }
        Expr::Loop(body) => {
            ExprT::Loop(Box::new(annotate_any(body)), TypeInfo::Any)
        }
        Expr::Break(expr) => {
            ExprT::Break(Box::new(annotate_any(expr)), TypeInfo::Any)
        }
        Expr::Set(name, expr) => {
            ExprT::Set(name.clone(), Box::new(annotate_any(expr)), TypeInfo::Any)
        }
        Expr::FunCall(name, args) => {
            let args_t = args.iter().map(|e| annotate_any(e)).collect();
            ExprT::FunCall(name.clone(), args_t, TypeInfo::Any)
        }
        Expr::Print(expr) => {
            ExprT::Print(Box::new(annotate_any(expr)), TypeInfo::Any)
        }
        Expr::Cast(target_type, expr) => {
            ExprT::Cast(target_type.clone(), Box::new(annotate_any(expr)), TypeInfo::Any)
        }
    }
}
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

    let is_annotated = defn.return_type.is_some() || defn.params.iter().any(|(_, t)| t.is_some());

    if is_annotated {
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
    }

    Ok(())
}


pub fn type_check(expr: &Expr, env_t:&HashMap<String, Box<TypeInfo>>, define_env_t:&mut MutHashMap<String, Box<TypeInfo>>, defns: &Vec<Defn> ) -> ExprT {

    let mut loop_ctx = vec![];
    type_check_helper(expr, env_t, define_env_t, defns, &mut loop_ctx ) 
}
pub fn type_check_helper(expr: &Expr, env_t:&HashMap<String, Box<TypeInfo>>, define_env_t:&mut MutHashMap<String, Box<TypeInfo>>, defns: &Vec<Defn>,  loop_ctx: &mut Vec<Vec<Box<TypeInfo>>> ) -> ExprT {
    
    match expr {
        Expr::Number(n) => ExprT::Number(*n, TypeInfo::Num),
        Expr::Boolean(b) => ExprT::Boolean(*b, TypeInfo::Bool),
        Expr::Id(name) => {
            // println!("Looking up identifier: {}", name);
            // println!("  env_t has: {:?}", env_t.keys().collect::<Vec<_>>());
            // println!("  define_env_t has: {:?}", define_env_t.keys().collect::<Vec<_>>());
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
        Expr::Let(bindings, body) => {
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
        Expr::Define(name, expr) => {
            let expr_t = type_check_helper(expr, &env_t, define_env_t, defns, loop_ctx);
            let expr_type = expr_t.get_type_info().clone();
            define_env_t.insert(name.clone(), Box::new(expr_type.clone()));
            ExprT::Define(name.clone(), Box::new(expr_t), expr_type)
        }
        Expr::Block(exprs) => {
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
        Expr::UnOp(op, subexpr) => match op {
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
        Expr::BinOp(op, left, right) => {
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
        Expr::If(cond, then_expr, else_expr) => {
            let cond_t = type_check_helper(cond, env_t, define_env_t, defns, loop_ctx);
            if !matches!(cond_t.get_type_info(), TypeInfo::Bool) {
                panic!("Type error: Expected Bool type for condition, got: {:?}", cond_t.get_type_info());
            }
            let then_t = type_check_helper(then_expr, env_t, define_env_t, defns, loop_ctx);
            let else_t = type_check_helper(else_expr, env_t, define_env_t, defns, loop_ctx);
            let if_type = union_type(then_t.get_type_info(), else_t.get_type_info());
            ExprT::If(Box::new(cond_t), Box::new(then_t), Box::new(else_t), if_type)
        }
        Expr::Loop(body) => {
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
            // println!("LOOP has type {:?}", t_info);
            ExprT::Loop(Box::new(body_t), t_info)
        }

            // find breaks dont know how tho...
        Expr::Break(expr) => {
            let expr_t: ExprT = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            let t_info = expr_t.get_type_info().clone();
            let curr_loop = loop_ctx.last_mut().expect("break outside of loop");
            curr_loop.push(Box::new(t_info.clone()));
            // println!("BREAK type: {:?}", t_info);
            ExprT::Break(Box::new(expr_t), t_info)
        }
        Expr::Set(name, expr) => {
            // Check if the variable exists in either environment
            // println!("SET: Before evaluating RHS, define_env_t has: {:?}", define_env_t.keys().collect::<Vec<_>>());
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            // println!("SET: After evaluating RHS, define_env_t has: {:?}", define_env_t.keys().collect::<Vec<_>>());
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
        Expr::FunCall(name, args) => {
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
        Expr::Print(expr) => {
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            let t_info = expr_t.get_type_info().clone();
            ExprT::Print(Box::new(expr_t), t_info)
        }
        Expr::Cast(target_type, expr) => {
            let expr_t = type_check_helper(expr, env_t, define_env_t, defns, loop_ctx);
            ExprT::Cast(target_type.clone(), Box::new(expr_t), target_type.clone())
        }
    }
}
