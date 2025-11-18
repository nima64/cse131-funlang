use crate::types::*;
use im::HashMap;
use prettydiff::format_table::new;

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
            ExprT::Break(Box::new(expr_t), TypeInfo::Any)
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
        // Expr::Cast(target_type, expr) => {
        //     let expr_t = type_check(expr, env_t);
        //     ExprT::Cast(target_type.clone(), Box::new(expr_t), target_type.clone())
        // }
    }
}
