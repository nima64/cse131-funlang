use crate::types::*;
use im::HashMap;

pub fn type_check_prog(prog: &Prog) -> Result<TypeInfo, String> {
    // Type check all function definitions
    for defn in &prog.defns {
        type_check_defn(defn)?;
    }

    // Type check the main expression with empty environment (functions are not in scope as values)
    let mut env_t = HashMap::new();
    env_t.insert("input".to_string(), Box::new(TypeInfo::Any));

    let main_t = type_check(&prog.main, &env_t);
    Ok(main_t.get_type_info().clone())
}

pub fn type_check_defn(defn: &Defn) -> Result<(), String> {
    let mut env_t = HashMap::new();

    // Check if function is annotated or un-annotated
    let is_annotated = defn.return_type.is_some() || defn.params.iter().any(|(_, t)| t.is_some());

    if is_annotated {
        // Annotated function: check that all parameters have types and return type is specified
        if defn.return_type.is_none() {
            return Err(format!("Type error: Function {} has typed parameters but no return type", defn.name));
        }

        let expected_return_type = defn.return_type.as_ref().unwrap();

        // Build environment with parameter types
        for (param_name, param_type_opt) in &defn.params {
            let param_type = param_type_opt.clone().unwrap_or(TypeInfo::Any);
            env_t.insert(param_name.clone(), Box::new(param_type));
        }

        // Type check the body
        let body_t = type_check(&defn.body, &env_t);
        let body_type = body_t.get_type_info();

        // Check that body type is a subtype of expected return type
        if !is_subtype(body_type, expected_return_type) {
            return Err(format!(
                "Type error: Function {} body has type {:?}, expected {:?}",
                defn.name, body_type, expected_return_type
            ));
        }
    } else {
        // Un-annotated function: all parameters are Any, body must be Any
        for (param_name, _) in &defn.params {
            env_t.insert(param_name.clone(), Box::new(TypeInfo::Any));
        }

        // Type check the body - it can have any type for un-annotated functions
        // We still type-check to catch errors but don't enforce Any
        let _body_t = type_check(&defn.body, &env_t);
    }

    Ok(())
}

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
        // T ∪ Any = Any, Any ∪ T = Any
        (Any, _) | (_, Any) => Any,

        // T ∪ T = T
        (Num, Num) => Num,
        (Bool, Bool) => Bool,
        (Nothing, Nothing) => Nothing,

        // T ∪ Nothing = T, Nothing ∪ T = T
        (Nothing, t) => t.clone(),
        (t, Nothing) => t.clone(),

        // Num ∪ Bool = Any, Bool ∪ Num = Any
        (Num, Bool) | (Bool, Num) => Any,
    }
}

pub fn type_check(expr: &Expr, env_t:&HashMap<String, Box<TypeInfo>>) -> ExprT {
    match expr {
        Expr::Number(n) => ExprT::Number(*n, TypeInfo::Num),
        Expr::Boolean(b) => ExprT::Boolean(*b, TypeInfo::Bool),
        Expr::Id(name) => {
            if let Some(type_info) = env_t.get(name) {
                ExprT::Id(name.clone(), *type_info.clone())
            }else if (name == "input"){
                ExprT::Id("input".to_string(), TypeInfo::Any)
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        }
        Expr::UnOp(op, subexpr) => match op {
            Op1::Add1 | Op1::Sub1 => {
                let subexpr_t = type_check(subexpr, env_t);
                if !matches!(subexpr_t.get_type_info(), TypeInfo::Num) {
                    panic!("Expected Num type, got: {:?}", subexpr_t);
                }
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Num)
            }
            Op1::IsNum => {
                let subexpr_t = type_check(subexpr, env_t);
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Bool)
            }
            Op1::IsBool => {
                let subexpr_t = type_check(subexpr, env_t);
                ExprT::UnOp(op.clone(), Box::new(subexpr_t), TypeInfo::Bool)
            }
        },
        Expr::Let(bindings, body) => {
            let mut t_bindings = vec![];
            let mut new_env = env_t.clone();
            let mut duplicate_binding = HashMap::new();

            for (var, val_expr) in bindings {
                if duplicate_binding.contains_key(var) {
                    panic!("Duplicate binding");
                }
                duplicate_binding.insert(var, 1);

                let val_t = type_check(val_expr, &new_env);
                let type_info = val_t.get_type_info().clone();

                t_bindings.push((var.clone(), val_t));
                new_env.insert(var.clone(), Box::new(type_info));
            }

            let body_t = type_check(body, &new_env);
            let body_type = body_t.get_type_info().clone();

            ExprT::Let(t_bindings, Box::new(body_t), body_type)
        }
        Expr::Define(name, expr) => {
            let expr_t = type_check(expr, env_t);
            ExprT::Define(name.clone(), Box::new(expr_t), TypeInfo::Any)
        }
        Expr::Block(exprs) => {
            let mut exprs_t = vec![];
            for expr in exprs {
                exprs_t.push(type_check(expr, env_t));
            }
            let block_type = match exprs_t.last() {
                Some(last_element) => last_element.get_type_info().clone(),
                None => panic!("empty block expression"),
            };
            ExprT::Block(exprs_t, block_type)
        }
        Expr::BinOp(op, left, right) => {
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    let left_t = type_check(left, env_t);
                    if !matches!(left_t.get_type_info(), TypeInfo::Num) {
                        panic!("Expected Num type for left operand, got: {:?}", left_t);
                    }

                    let right_t = type_check(right, env_t);
                    if !matches!(right_t.get_type_info(), TypeInfo::Num) {
                        panic!("Expected Num type for right operand, got: {:?}", right_t);
                    }
                    ExprT::BinOp(op.clone(), Box::new(left_t), Box::new(right_t), TypeInfo::Num)
                }
                Op2::Less | Op2::Greater | Op2::LessEqual | Op2::GreaterEqual | Op2::Equal => {
                    let left_t = type_check(left, env_t);
                    let right_t = type_check(right, env_t);
                    ExprT::BinOp(op.clone(), Box::new(left_t), Box::new(right_t), TypeInfo::Bool)
                }
            }
        }
        Expr::If(cond, then_expr, else_expr) => {
            let cond_t = type_check(cond, env_t);
            if !matches!(cond_t.get_type_info(), TypeInfo::Bool) {
                panic!("Type error: Expected Bool type for condition, got: {:?}", cond_t.get_type_info());
            }
            let then_t = type_check(then_expr, env_t);
            let else_t = type_check(else_expr, env_t);
            let if_type = union_type(then_t.get_type_info(), else_t.get_type_info());
            ExprT::If(Box::new(cond_t), Box::new(then_t), Box::new(else_t), if_type)
        }
        Expr::Loop(body) => {
            let body_t = type_check(body, env_t);
            ExprT::Loop(Box::new(body_t), TypeInfo::Any)
        }
        Expr::Break(expr) => {
            let expr_t = type_check(expr, env_t);
            ExprT::Break(Box::new(expr_t), TypeInfo::Nothing)
        }
        Expr::Set(name, expr) => {
            // Check if the variable exists in the environment
            if !env_t.contains_key(name) {
                panic!("Unbound variable identifier {}", name);
            }
            let expr_t = type_check(expr, env_t);
            let expr2_t = env_t.get(name).unwrap();

            if !is_subtype(expr_t.get_type_info(), &(*expr2_t)) {
                panic!("Type error: {:?} is not a subtype of {:?}", expr_t.get_type_info(), *expr2_t)
            }

            ExprT::Set(name.clone(), Box::new(expr_t), TypeInfo::Any)
        }
        Expr::FunCall(name, args) => {
            let mut args_t = vec![];
            for arg in args {
                args_t.push(type_check(arg, env_t));
            }
            ExprT::FunCall(name.clone(), args_t, TypeInfo::Any)
        }
        Expr::Print(expr) => {
            let expr_t = type_check(expr, env_t);
            ExprT::Print(Box::new(expr_t), TypeInfo::Any)
        }
        Expr::Cast(target_type, expr) => {
            let expr_t = type_check(expr, env_t);
            ExprT::Cast(target_type.clone(), Box::new(expr_t), target_type.clone())
        }
    }
}
