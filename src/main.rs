mod compiler;
mod assembly;
mod parser;
mod common;
mod types;
mod typechecker;
mod compile_me;
mod optimizer;

use assembly::*;
use compiler::*;
use parser::*;
use common::*;

use sexp::*;
use std::env;
use std::fs::File;
use std::io::{self, *};
use std::collections::HashMap;
use crate::types::{Prog, TypeInfo};
use crate::typechecker::*;
use crate::optimizer::*;


fn run_jit(in_name: &str, input_arg: &str, typecheck_enabled: bool) -> std::io::Result<()> {
    let input = parse_input(input_arg);

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Wrap in parentheses to create a program
    in_contents = format!("({})", in_contents);
    let sexpr = parse(&in_contents).unwrap();
    let mut prog = parse_prog(&sexpr);
    
    let mut env_t = im::HashMap::new();
    env_t.insert("input".to_string(), Box::new(TypeInfo::Any));
    let mut define_env_t = std::collections::HashMap::new();

    if typecheck_enabled {
        match annotate_program(&mut prog, &mut HashMap::new()) {
            Ok(_) => {},
            Err(err) => {
                eprintln!("Type Error: {}", err);
                std::process::exit(1);
            }
        }
        optimize_program(&mut prog);
    }

    let instrs = compile_prog(&prog, &mut im::HashMap::new(), &mut define_env_t, true, typecheck_enabled);

    let result = jit_code_input(&instrs, input);
    println!("{}", format_result(result));
    Ok(())
}

fn run_aot(in_name: &str, out_name: &str, typecheck_enabled: bool) -> std::io::Result<()> {
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    in_contents = format!("({})", in_contents);

    let sexpr = parse(&in_contents).unwrap();
    let mut prog = parse_prog(&sexpr);

    let mut env_t = im::HashMap::new();
    env_t.insert("input".to_string(), Box::new(TypeInfo::Any));
    let mut define_env_t = std::collections::HashMap::new();

    if typecheck_enabled {
        match annotate_program(&mut prog, &mut HashMap::new()) {
            Ok(_) => {},
            Err(err) => {
                eprintln!("Type Error: {}", err);
                std::process::exit(1);
            }
        }
    }

    let instrs = compile_prog(&prog, &mut im::HashMap::new(), &mut define_env_t, false, typecheck_enabled);
    let result = instrs_to_string(&instrs);

        let asm_program = format!(
                "
section .text
extern snek_error
extern print_fun
global our_code_starts_here
our_code_starts_here:
{}
    jmp done
overflow_error:
    mov rdi, 2
    jmp error_common
type_mismatch_error:
    mov rdi, 1
    jmp error_common
type_error_arithmetic:
    mov rdi, 3
    jmp error_common
bad_cast_error:
    mov rdi, 4
error_common:
    call snek_error
    jmp done
print_fun_external:
    sub rsp, 8 ; alignment for 16 bytes to prevent segfaulting
    call print_fun
    add rsp, 8
    ret
done:
    ret
                        ",
                result
        );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    Ok(())
}

// Infer the type of input based on its value
fn infer_input_type(input_arg: &str) -> TypeInfo {
    let input = parse_input(input_arg);
    // Check if it's a boolean (tagged with 1 in lower bit)
    if (input & 1) == 1 {
        TypeInfo::Bool
    } else {
        TypeInfo::Num
    }
}

// Just type check and print the type (for -t)
fn run_t(in_name: &str) -> std::io::Result<()> {
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    in_contents = format!("({})", in_contents);

    let sexpr = parse(&in_contents).unwrap();
    let mut prog = parse_prog(&sexpr);

    let mut env_t = im::HashMap::new();
    env_t.insert("input".to_string(), Box::new(TypeInfo::Any));
    // let mut define_env_t = std::collections::HashMap::new();

    match annotate_program(&mut prog, &mut HashMap::new()) {
        Ok(_) => {println!("{:?}", &prog.main.get_type_info()); return Ok(())},
        Err(err) => {eprintln!("Type Error: {}", err); return Ok(());}
    }
}

fn run_repl(typecheck_enabled: bool) {
    println!("opening repl");
        let mut repl_env: im::HashMap<String, Box<i64>> = im::HashMap::new();
        let mut accumulated_defns: Vec<types::Defn> = vec![];
        let mut define_env_t = std::collections::HashMap::new();
        let mut global_var_env= HashMap::new();

        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim() == "quit" {
                break;
            }

            if input.trim().is_empty() {
                continue;
            }

            input = format!("({})", input);

            let sexpr = match parse(&input) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Parse error: {:?}", e);
                    continue;
                }
            };
            let parsed_prog = parse_prog(&sexpr);

            // Accumulate function definitions
            accumulated_defns.extend(parsed_prog.defns);

            // Create a new Prog with all accumulated defns and the new main expression
            let mut prog = Prog {
                defns: accumulated_defns.clone(),
                main: parsed_prog.main,
            };

            if typecheck_enabled {
                match annotate_program(&mut prog, &mut global_var_env) {
                    Ok(_) => {
                        // define_env_t = temp_define_env_t;
                        let instrs = compile_prog(&prog, &mut repl_env, &mut define_env_t, false, typecheck_enabled);

                        if !instrs.is_empty() {
                            let result = jit_code(&instrs);
                            println!("{}", format_result(result));
                        }
                    }
                    Err(err) => {
                        println!("Type Error: {}", err);
                    }
                }
            }
            else {
                let instrs = compile_prog(&prog, &mut repl_env, &mut define_env_t, false, typecheck_enabled);
                if !instrs.is_empty() {
                    let result = jit_code(&instrs);
                    println!("{}", format_result(result));
                }
            }
        }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Check for flags - type checking flags first
    let enable_typecheck = args.iter().any(|arg| arg.starts_with("-t"));

    // Regular flags
    let use_jit = args.iter().any(|arg| arg == "-e" || arg == "-te");
    let use_aot = args.iter().any(|arg| arg == "-c" || arg == "-tc");
    let use_repl = args.iter().any(|arg| arg == "-i" || arg == "-ti");
    let use_g = args.iter().any(|arg| arg == "-g" || arg == "-tg");

    if use_g {
        // Combined AOT + JIT compilation path (-g)
        let in_name = &args[2];
        let out_name = &args[3];
        let input_arg = if args.len() >= 5 { &args[4] } else { "false" };

        run_aot(in_name, out_name, enable_typecheck)?;
        run_jit(in_name, input_arg, enable_typecheck)?;
    } else if use_jit {
        let in_name = &args[2];
        let input_arg = if args.len() >= 4 { &args[3] } else { "false" };

        run_jit(in_name, input_arg, enable_typecheck)?;
    } else if use_repl {
        run_repl(enable_typecheck);
    } else if use_aot {
        let in_name = &args[2];
        let out_name = &args[3];

        run_aot(in_name, out_name, enable_typecheck)?;
    } else if enable_typecheck {
        let in_name = &args[2];
        run_t(in_name)?;
    }
    Ok(())
}
