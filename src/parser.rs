use std::collections::HashSet;

use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use crate::common::*;
use crate::types::*;


pub fn parse_expr(s: &Sexp) -> ExprT {
    match s {
        Sexp::Atom(I(n)) => {
            if !(*n >= -2_i64.pow(62) && *n <= 2_i64.pow(62) - 1) {
                panic!("not a valid number must be an integer between -2^62 and 2^62-1");
            }
            ExprT::Number(tag_number(*n), TypeInfo::Any)
        }
        Sexp::Atom(S(name)) => {
            /* Check is boolean */
            if name == "true" {
                return ExprT::Boolean(true, TypeInfo::Any);
            } else if name == "false" {
                return ExprT::Boolean(false, TypeInfo::Any);
            }
            ExprT::Id(name.to_string(), TypeInfo::Any)
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    ExprT::UnOp(Op1::Add1, Box::new(parse_expr(e)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    ExprT::UnOp(Op1::Sub1, Box::new(parse_expr(e)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    ExprT::UnOp(Op1::IsNum, Box::new(parse_expr(e)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    ExprT::UnOp(Op1::IsBool, Box::new(parse_expr(e)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), e] if op == "break" => ExprT::Break(Box::new(parse_expr(e)), TypeInfo::Any),
                [Sexp::Atom(S(op)), e] if op == "loop" => ExprT::Loop(Box::new(parse_expr(e)), TypeInfo::Any),
                [Sexp::Atom(S(op)), e] if op == "print" => ExprT::Print(Box::new(parse_expr(e)), TypeInfo::Any),
                [Sexp::Atom(S(op)), Sexp::Atom(S(type_name)), e] if op == "cast" => {
                    let target_type = match type_name.as_str() {
                        "Num" => TypeInfo::Num,
                        "Bool" => TypeInfo::Bool,
                        "Nothing" => TypeInfo::Nothing,
                        "Any" => TypeInfo::Any,
                        _ => panic!("Invalid type: {}", type_name),
                    };
                    ExprT::Cast(target_type.clone(), Box::new(parse_expr(e)), target_type)
                }
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => ExprT::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => ExprT::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => ExprT::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => ExprT::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => ExprT::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => ExprT::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => ExprT::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => ExprT::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    TypeInfo::Any,
                ),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid: block needs at least one expression");
                    }
                    ExprT::Block(exprs.iter().map(parse_expr).collect(), TypeInfo::Any)
                }

                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    // Map each binding to (var_name, parsed_expr) tuple
                    let parsed_bindings: Vec<(String, ExprT)> = bindings
                        .iter()
                        .map(|binding| match binding {
                            Sexp::List(pair) => match &pair[..] {
                                [Sexp::Atom(S(var)), val] => (var.to_string(), parse_expr(val)),
                                _ => panic!("Invalid binding: expected (variable value)"),
                            },
                            _ => panic!("Invalid binding: expected a list"),
                        })
                        .collect();

                    ExprT::Let(parsed_bindings, Box::new(parse_expr(body)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "define" => {
                    ExprT::Define(name.to_string(), Box::new(parse_expr(e)), TypeInfo::Any)
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    ExprT::Set(name.to_string(), Box::new(parse_expr(e)), TypeInfo::Any)
                }

                [Sexp::Atom(S(op)), cond, then_expr, else_expr] if op == "if" => ExprT::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(then_expr)),
                    Box::new(parse_expr(else_expr)),
                    TypeInfo::Any,
                ),

                // Function call: (<name> <expr>*)
                [Sexp::Atom(S(name)), args @ ..] => {
                    let parsed_args = args.iter().map(|arg| parse_expr(arg)).collect();
                    ExprT::FunCall(name.to_string(), parsed_args, TypeInfo::Any)
                }

                _ => panic!("parse error!"),
            }
        }
        _ => panic!("parse error"),
    }
}

pub fn parse_defn(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => {
            // Check for annotated function: (fun (name (param : Type) ...) -> RetType body)
            // or un-annotated function: (fun (name param ...) body)
            match &vec[..] {
                [Sexp::Atom(S(keyword)), Sexp::List(signature), Sexp::Atom(S(arrow)), ret_type_sexp, body]
                    if keyword == "fun" && arrow == "->" => {
                    // Annotated function with return type
                    match &signature[..] {
                        [Sexp::Atom(S(name)), params @ ..] => {
                            let parsed_params = parse_typed_params(params);
                            let ret_type = parse_type(ret_type_sexp);

                            Defn {
                                name: name.to_string(),
                                params: parsed_params,
                                return_type: ret_type,
                                body: Box::new(parse_expr(body)),
                            }
                        }
                        _ => panic!("Invalid: function definition must have a name"),
                    }
                }
                [Sexp::Atom(S(keyword)), Sexp::List(signature), body] if keyword == "fun" => {
                    // Un-annotated function or partially annotated
                    match &signature[..] {
                        [Sexp::Atom(S(name)), params @ ..] => {
                            let parsed_params = parse_params(params);

                            Defn {
                                name: name.to_string(),
                                params: parsed_params,
                                return_type: TypeInfo::Any,
                                body: Box::new(parse_expr(body)),
                            }
                        }
                        _ => panic!("Invalid: function definition must have a name"),
                    }
                }
                _ => panic!("Invalid: expected function definition (fun ...)"),
            }
        }
        _ => panic!("Invalid: function definition must be a list"),
    }
}

fn parse_type(s: &Sexp) -> TypeInfo {
    match s {
        Sexp::Atom(S(type_name)) => match type_name.as_str() {
            "Num" => TypeInfo::Num,
            "Bool" => TypeInfo::Bool,
            "Nothing" => TypeInfo::Nothing,
            "Any" => TypeInfo::Any,
            _ => panic!("Invalid type: {}", type_name),
        },
        _ => panic!("Invalid: expected a type name"),
    }
}

fn parse_params(params: &[Sexp]) -> Vec<Arg> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();

    for param in params {
        match param {
            Sexp::Atom(S(param_name)) => {
                // Un-annotated parameter - default to Any
                if !seen.insert(param_name.to_string()) {
                    panic!("Duplicate Argument");
                }
                result.push(Arg {
                    name: param_name.to_string(),
                    ann_type: TypeInfo::Any,
                });
            }
            _ => panic!("Invalid: function parameter must be an identifier"),
        }
    }

    result
}

fn parse_typed_params(params: &[Sexp]) -> Vec<Arg> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();

    for param in params {
        match param {
            Sexp::List(param_parts) => match &param_parts[..] {
                [Sexp::Atom(S(param_name)), Sexp::Atom(S(colon)), type_sexp] if colon == ":" => {
                    // Annotated parameter: (x : Type)
                    if !seen.insert(param_name.to_string()) {
                        panic!("Duplicate Argument");
                    }
                    let param_type = parse_type(type_sexp);
                    result.push(Arg {
                        name: param_name.to_string(),
                        ann_type: param_type,
                    });
                }
                _ => panic!("Invalid: expected (param : Type)"),
            },
            Sexp::Atom(S(param_name)) => {
                // Un-annotated parameter in annotated function - default to Any
                if !seen.insert(param_name.to_string()) {
                    panic!("Duplicate Argument");
                }
                result.push(Arg {
                    name: param_name.to_string(),
                    ann_type: TypeInfo::Any,
                });
            }
            _ => panic!("Invalid: function parameter format"),
        }
    }

    result
}

pub fn parse_prog(s: &Sexp) -> Prog {
    match s {
        Sexp::List(items) => {
            let mut defns: Vec<Defn> = Vec::new();
            let mut main_expr = None;

            for item in items {
                match item {
                    Sexp::List(inner) => {
                        if let Some(Sexp::Atom(S(keyword))) = inner.first() {
                            if keyword == "fun" {
                                let defn = parse_defn(item);
                                if defns.iter().any(|d| d.name == defn.name) {
                                    panic!("Multiple functions are defined with the same name");
                                }
                                defns.push(defn);
                                continue;
                            }
                        }
                        // If we get here, it's not a function definition, so it's the main expr
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                    _ => {
                        // Any other expression (atoms, etc.) is the main expression
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                }
            }

            Prog {
                defns,
                main: Box::new(
                    main_expr
                        .unwrap_or_else(|| ExprT::Boolean(false, TypeInfo::Any)),
                ),
            }
        }
        _ => {
            // Single expression, no function definitions
            Prog {
                defns: Vec::new(),
                main: Box::new(parse_expr(s)),
            }
        }
    }
}
