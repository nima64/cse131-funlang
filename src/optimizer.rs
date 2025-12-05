use crate::types::*;
use crate::common::*;
use im::HashMap;

fn has_set(expr: &ExprT, x: String) -> bool {
    match expr {
        ExprT::Set(name, _, _) => *name == x,
        ExprT::UnOp(_, e, _)
        | ExprT::Print(e, _)
        | ExprT::Loop(e, _)
        | ExprT::Break(e, _) 
        | ExprT::Define(_, e, _)
        | ExprT::Cast(_, e, _) => has_set(e, x.clone()),
        ExprT::BinOp(_, e1, e2, _) => has_set(e1, x.clone()) || has_set(e2, x.clone()),
        ExprT::Let(bindings, eb, _) => {
            for (_, bind) in bindings {
                if has_set(bind, x.clone()) {
                    return true;
                }
            }
            has_set(eb, x.clone())
        },
        ExprT::If(cond_e, then_e, else_e, _) =>
            has_set(cond_e, x.clone()) || has_set(then_e, x.clone()) || has_set(else_e, x.clone()),
        ExprT::Block(exprs, _)
        | ExprT::FunCall(_, exprs, _) => {
            for e in exprs {
                if has_set(e, x.clone()) {
                    return true;
                }
            }
            return false;
        },
        ExprT::Number(_, _) | ExprT::Boolean(_, _) | ExprT::Id(_, _) => false
    }
}

fn is_value(e: &ExprT) -> bool {
    match e {
        ExprT::Number(_, _) | ExprT::Boolean(_, _) => true,
        _ => false,
    }
}

fn i_to_val(i: i64) -> ExprT {
    
}

pub fn optimize(e: &ExprT, env: HashMap<String, ExprT>) -> ExprT {
    match e {
        ExprT::Number(_, _) | ExprT::Boolean(_, _) => {e.clone()}
        // Constant Folding Optimizations:

        // TODO: May need overflow checks?
        ExprT::UnOp(op, subexpr, t) => {
            let subexpr_folded = optimize(subexpr, env.clone());
            match (op, subexpr_folded.clone()) {
                (Op1::Add1, ExprT::Number(n, _)) => ExprT::Number(n + 1, TypeInfo::Num),
                (Op1::Sub1, ExprT::Number(n, _)) => ExprT::Number(n - 1, TypeInfo::Num),
                (Op1::IsNum, ExprT::Boolean(_, _)) | (Op1::IsBool, ExprT::Number(_, _)) => ExprT::Boolean(false, TypeInfo::Bool),
                (Op1::IsNum, ExprT::Number(_, _)) | (Op1::IsBool, ExprT::Boolean(_, _)) => ExprT::Boolean(true, TypeInfo::Bool),
                _ => ExprT::UnOp(op.clone(), Box::new(subexpr_folded), t.clone())
            }
        }
        ExprT::Print(e, t) => {ExprT::Print(Box::new(optimize(e, env)), t.clone())}
        ExprT::BinOp(op, e1, e2, t) => {
            let e1_folded = optimize(e1, env.clone());
            let e2_folded = optimize(e2, env.clone());
            match (op, e1_folded.clone(), e2_folded.clone()) {
                (Op2::Plus, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Number(n1 + n2, TypeInfo::Num),
                (Op2::Plus, e_folded, ExprT::Number(1, _)) | (Op2::Plus, ExprT::Number(1, _), e_folded) => ExprT::UnOp(Op1::Add1, Box::new(e_folded), t.clone()),

                (Op2::Minus, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Number(n1 - n2, TypeInfo::Num),
                (Op2::Minus, e_folded, ExprT::Number(1, _)) => ExprT::UnOp(Op1::Sub1, Box::new(e_folded), t.clone()),

                (Op2::Times, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Number(n1 * n2, TypeInfo::Num),

                (Op2::Plus, e_folded, ExprT::Number(0, _))
                | (Op2::Minus, e_folded, ExprT::Number(0, _))
                | (Op2::Times, e_folded, ExprT::Number(1, _)) => e_folded,

                (Op2::Plus, ExprT::Number(0, _), e_folded) 
                | (Op2::Times, ExprT::Number(1, _), e_folded) => e_folded,

                (Op2::Equal, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Boolean(n1 == n2, TypeInfo::Bool),
                (Op2::Equal, ExprT::Boolean(b1, _), ExprT::Boolean(b2, _)) => ExprT::Boolean(b1 == b2, TypeInfo::Bool),
                (Op2::Less, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Boolean(n1 < n2, TypeInfo::Bool),
                (Op2::LessEqual, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Boolean(n1 <= n2, TypeInfo::Bool),
                (Op2::Greater, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Boolean(n1 > n2, TypeInfo::Bool),
                (Op2::GreaterEqual, ExprT::Number(n1, _), ExprT::Number(n2, _)) => ExprT::Boolean(n1 >= n2, TypeInfo::Bool),

                _ => ExprT::BinOp(op.clone(), Box::new(e1_folded), Box::new(e2_folded), t.clone())
            }
        }

        // Constant Propogation Optimizations:
        ExprT::Id(name, t) => {
            if let Some(val) = env.get(name) {val.clone()} else {e.clone()}
        }
        ExprT::Let(bindings, body, t) => {
            let mut new_env = env.clone();
            let mut optimized_bindings = Vec::new();
            let mut all_values = true;
            for (name, bind) in bindings {
                let optimized_bind = optimize(bind, new_env.clone());
                if is_value(&optimized_bind) && !has_set(&body, name.clone()) {
                    new_env.insert(name.clone(), optimized_bind.clone());
                }
                else {
                    all_values = false;
                }
                optimized_bindings.push((name.clone(), optimized_bind));
            }

            let optimized_body = optimize(body, new_env);
            if all_values{
                optimized_body
            } else {
                ExprT::Let(optimized_bindings, Box::new(optimized_body), t.clone())
            }
        }
        
        // If Statement Dead Code Optimization:
        ExprT::If(cond_e, then_e, else_e, t) => {
            let optimized_cond = optimize(cond_e, env.clone());
            match optimized_cond {
                ExprT::Boolean(true, _) => optimize(then_e, env.clone()),
                ExprT::Boolean(false, _) => optimize(else_e, env.clone()),
                _ => ExprT::If(Box::new(optimized_cond), Box::new(optimize(then_e, env.clone())), Box::new(optimize(else_e, env.clone())), t.clone())
            }
        }

        // Cast optimization
        ExprT::Cast(target, e, t) => {
            let optimized_e = optimize(e, env.clone());
            if optimized_e.get_type_info().is_subtype_of(t) {
                optimized_e
            } else {
                ExprT::Cast(target.clone(), Box::new(optimized_e), t.clone())
            }
        }

        // UNOPTIMIZED other expressions (replace when specific optimizaitons is implemented, otherwise just optimizes recursively)
        ExprT::Block(exprs, t) => {
            let optimized_exprs: Vec<ExprT> = exprs.iter().map(|e| optimize(e, env.clone())).collect();
            ExprT::Block(optimized_exprs, t.clone())
        }
        ExprT::Break(e, t) => {ExprT::Break(Box::new(optimize(e, env.clone())), t.clone())}
        ExprT::Loop(e, t) => {ExprT::Loop(Box::new(optimize(e, env.clone())), t.clone())}
        ExprT::Define(name, e, t) => {ExprT::Define(name.clone(), Box::new(optimize(e, env.clone())), t.clone())}
        ExprT::Set(name, e, t) => {ExprT::Set(name.clone(), Box::new(optimize(e, env.clone())), t.clone())}
        ExprT::FunCall(name, args, t) => {
            let optimized_args: Vec<ExprT> = args.iter().map(|e| optimize(e, env.clone())).collect();
            ExprT::FunCall(name.clone(), optimized_args, t.clone())
        }
    }
}

pub fn optimize_program(prog: &mut Prog) {
    let mut new_defns = Vec::new();
    for defn in &prog.defns {
        let env = HashMap::new();
        let optimized_body = optimize(&defn.body, env);
        let optimized_defn = Defn {
            name: defn.name.clone(),
            params: defn.params.clone(),
            body: Box::new(optimized_body),
            return_type: defn.return_type.clone()
        };
        new_defns.push(optimized_defn);
    }
    prog.defns = new_defns;
    prog.main = Box::new(optimize(&prog.main, HashMap::new()));
}