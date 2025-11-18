mod compiler;
mod assembly;
mod parser;
mod common;
mod types;
mod typechecker;

use assembly::*;
use compiler::*;
use parser::*;
use common::*;

use sexp::*;
use std::env;
use std::fs::File;
use std::io;
use std::io::*;
use im::HashMap;

use crate::typechecker::type_check;
use crate::types::Prog;


fn run_jit(in_name: &str, input_arg: &str) -> std::io::Result<()> {
    let input = parse_input(input_arg);

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Wrap in parentheses to create a program
    in_contents = format!("({})", in_contents);
    let sexpr = parse(&in_contents).unwrap();
    let prog = parse_prog(&sexpr);
    let instrs = compile_prog(&prog, &mut HashMap::new());

    let result = jit_code_input(&instrs, input);
    println!("{}", format_result(result));
    Ok(())
}

fn run_aot(in_name: &str, out_name: &str) -> std::io::Result<()> {
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    in_contents = format!("({})", in_contents);

    let sexpr = parse(&in_contents).unwrap();
    let prog = parse_prog(&sexpr);
    
    let progTyped = type_check(&prog.main, &HashMap::new());
    println!("typed program body {:?}", progTyped);

    let instrs = compile_prog(&prog, &mut HashMap::new());
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

fn run_repl() {
        let mut repl_env: HashMap<String, Box<i64>> = HashMap::new();
        let mut accumulated_defns: Vec<types::Defn> = vec![];

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

            let sexpr = parse(&input).unwrap();
            let parsed_prog = parse_prog(&sexpr);

            // Accumulate function definitions
            accumulated_defns.extend(parsed_prog.defns);

            // Create a new Prog with all accumulated defns and the new main expression
            let prog = Prog {
                defns: accumulated_defns.clone(),
                main: parsed_prog.main,
            };

            let instrs = compile_prog(&prog, &mut repl_env);

            if !instrs.is_empty() {
                let result = jit_code(&instrs);
                println!("{}", format_result(result));
            }
        }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Check for flags
    let use_jit = args.iter().any(|arg| arg == "-e");
    let use_aot = args.iter().any(|arg| arg == "-c");
    let use_repl = args.iter().any(|arg| arg == "-i");
    let use_g = args.iter().any(|arg| arg == "-g");

    if use_g {
        // Combined AOT + JIT compilation path (-g)
        let in_name = &args[2];
        let out_name = &args[3];
        let input_arg = if args.len() >= 5 { &args[4] } else { "false" };

        run_aot(in_name, out_name)?;
        run_jit(in_name, input_arg)?;
    } else if use_jit {
        let in_name = &args[2];
        let input_arg = if args.len() >= 4 { &args[3] } else { "false" };

        run_jit(in_name, input_arg)?;
    } else if use_repl {
        run_repl();
    } else if use_aot {
        let in_name = &args[2];
        let out_name = &args[3];

        run_aot(in_name, out_name)?;
    }
    Ok(())
}
