use std::collections::HashSet;

use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use crate::common::*;
use crate::types::*;


pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if !(*n >= -2_i64.pow(62) && *n <= 2_i64.pow(62) - 1) {
                panic!("not a valid number must be an integer between -2^62 and 2^62-1");
            }
            Expr::Number(*n)
        }
        Sexp::Atom(S(name)) => {
            /* Check is boolean */
            if name == "true" {
                return Expr::Boolean(true);
            } else if name == "false" {
                return Expr::Boolean(false);
            }
            Expr::Id(name.to_string())
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(type_name)), e] if op == "cast" => {
                    let target_type = match type_name.as_str() {
                        "Num" => TypeInfo::Num,
                        "Bool" => TypeInfo::Bool,
                        "Nothing" => TypeInfo::Nothing,
                        "Any" => TypeInfo::Any,
                        _ => panic!("Invalid type: {}", type_name),
                    };
                    Expr::Cast(target_type, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid: block needs at least one expression");
                    }
                    Expr::Block(exprs.iter().map(parse_expr).collect())
                }

                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    // Map each binding to (var_name, parsed_expr) tuple
                    let parsed_bindings: Vec<(String, Expr)> = bindings
                        .iter()
                        .map(|binding| match binding {
                            Sexp::List(pair) => match &pair[..] {
                                [Sexp::Atom(S(var)), val] => (var.to_string(), parse_expr(val)),
                                _ => panic!("Invalid binding: expected (variable value)"),
                            },
                            _ => panic!("Invalid binding: expected a list"),
                        })
                        .collect();

                    Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "define" => {
                    Expr::Define(name.to_string(), Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                }

                [Sexp::Atom(S(op)), cond, then_expr, else_expr] if op == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(then_expr)),
                    Box::new(parse_expr(else_expr)),
                ),

                // Function call: (<name> <expr>*)
                [Sexp::Atom(S(name)), args @ ..] => {
                    let parsed_args = args.iter().map(|arg| parse_expr(arg)).collect();
                    Expr::FunCall(name.to_string(), parsed_args)
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
                                return_type: Some(ret_type),
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
                                return_type: None,
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

fn parse_params(params: &[Sexp]) -> Vec<(String, Option<TypeInfo>)> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();

    for param in params {
        match param {
            Sexp::Atom(S(param_name)) => {
                // Un-annotated parameter
                if !seen.insert(param_name.to_string()) {
                    panic!("Duplicate Argument");
                }
                result.push((param_name.to_string(), None));
            }
            _ => panic!("Invalid: function parameter must be an identifier"),
        }
    }

    result
}

fn parse_typed_params(params: &[Sexp]) -> Vec<(String, Option<TypeInfo>)> {
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
                    result.push((param_name.to_string(), Some(param_type)));
                }
                _ => panic!("Invalid: expected (param : Type)"),
            },
            Sexp::Atom(S(param_name)) => {
                // Un-annotated parameter in annotated function
                if !seen.insert(param_name.to_string()) {
                    panic!("Duplicate Argument");
                }
                result.push((param_name.to_string(), None));
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
                        .unwrap_or_else(|| Expr::Boolean(false)),
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
