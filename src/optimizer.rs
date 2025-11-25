use crate::types::*;
use crate::common::*;

pub fn optimize(e: &ExprT) -> ExprT {
    match e {
        ExprT::UnOp(op, subexpr, t) => {
            let subexpr_folded = optimize(subexpr);
            match (op, subexpr_folded.clone()) {
                (Op1::Add1, ExprT::Number(n, _)) => ExprT::Number(n + 1, TypeInfo::Num),
                (Op1::Sub1, ExprT::Number(n, _)) => ExprT::Number(n - 1, TypeInfo::Num),
                (Op1::IsNum, ExprT::Boolean(_, _)) | (Op1::IsBool, ExprT::Number(_, _)) => ExprT::Boolean(false, TypeInfo::Bool),
                (Op1::IsNum, ExprT::Number(_, _)) | (Op1::IsBool, ExprT::Boolean(_, _)) => ExprT::Boolean(true, TypeInfo::Bool),
                _ => ExprT::UnOp(op.clone(), Box::new(subexpr_folded), t.clone())
            }
        }
        ExprT::BinOp(op, e1, e2, t) => {
            let e1_folded = optimize(e1);
            let e2_folded = optimize(e2);
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
        _ => e.clone()
    }
}