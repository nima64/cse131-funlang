// Common shared code between runtime and compiler
// This module has NO external dependencies

pub const TRUE_TAGGED: i64 = 3;
pub const FALSE_TAGGED: i64 = 1;
pub const BOOL_TAG: i64 = 1;

pub fn get_tag(n: i64) -> i64 {
    n & 1
}

pub fn tag_number(n: i64) -> i64 {
    n << 1
}

pub fn untag_number(n: i64) -> i64 {
    n >> 1
}

pub fn format_result(res: i64) -> String {
    let tag = get_tag(res);
    if tag == BOOL_TAG {
        if res == TRUE_TAGGED {
            return "true".to_string();
        } else {
            return "false".to_string();
        }
    }
    return untag_number(res).to_string();
}

pub fn parse_input(input: &str) -> i64 {
    let trimmed = input.trim();
    let res = trimmed.parse::<i64>();
    if let Ok(n) = res {
        if n > 2_i64.pow(62) - 1 ||  n < -2_i64.pow(62) {
            panic!("number must be between {} and {}", -2_i64.pow(62), 2_i64.pow(62)-1);
        }
        return tag_number(n);
    }
    match trimmed {
        "true" => TRUE_TAGGED,
        "false" => FALSE_TAGGED,
        "" => FALSE_TAGGED,
        _ => panic!("not a valid value {}", input),
    }
}

pub extern "C" fn snek_error(errorcode: i64) {
    match errorcode {
        1 => {
            eprintln!("cannot compare bool with an int");
            std::process::exit(1);
        }
        2 => {
            eprintln!("integer overflow");
            std::process::exit(1);
        }
        3 => {
            eprintln!("cannot do arithmetic on non integers");
            std::process::exit(1);
        }
        4 => {
            eprintln!("bad cast");
            std::process::exit(1);
        }
        _ => {
            eprintln!("invalid error code {}!", errorcode);
            std::process::exit(1);
        }
    }
}

pub extern "C" fn print_fun(val: i64) -> i64 {
    println!("{}", format_result(val));
    val
}