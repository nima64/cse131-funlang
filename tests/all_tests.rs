mod infra;

// Your tests go here!
success_tests! {
    // Type checking tests
    type_test_1_typed: { file: "type_test_1_typed", input: "", expected: "25" },
    type_test_2_unannotated_cast: { file: "type_test_2_unannotated_cast", input: "", expected: "25" },
    type_test_5_loop_any: { file: "type_test_5_loop_any", input: "", expected: "1" },
    type_test_6_loop_num: { file: "type_test_6_loop_num", input: "50", expected: "100" },
    type_test_7_if_any: { file: "type_test_7_if_any", input: "42", expected: "42" },
    type_test_9_input_cast: { file: "type_test_9_input_cast", input: "5", expected: "8" },

//     test_input: { file: "input", input: "2", expected: "2" },
//     nested_loop: { file: "nested_loop", input: "", expected: "-6" },
//     binop_plus: { file: "plus", input: "", expected: "8" },
//     binop_minus: { file: "minus", input: "", expected: "6" },
//     binop_times: { file: "times", input: "", expected: "42" },
//     binop_less: { file: "less", input: "", expected: "true" },
//     binop_greater: { file: "greater", input: "", expected: "true" },
//     binop_less_equal: { file: "less_equal", input: "", expected: "true" },
//     binop_greater_equal: { file: "greater_equal", input: "", expected: "true" },
//     binop_equal: { file: "equal", input: "", expected: "true" },
//     unop_add1: { file: "add1", input: "", expected: "4" },
//     unop_sub1: { file: "sub1", input: "", expected: "-4" },
//     fun_fact: {file:"fact", input: "9", expected: "362880"},
//     fun_iseven: {file:"fun_iseven", input: "99", expected: "99\nfalse\nfalse"},
//     fun_print_iseven: {file:"fun_print_iseven", input: "99", expected: "false\nfalse"},
//     fun_print: {file:"print", input: "", expected: "3\n3"},
//     fun_chain: {file:"fun_chain", input: "", expected: "19
// 19"},
//     fun_mutual_recursion: {file:"fun_mutual_recursion", input: "", expected: "25\n25"},
//     fun_mutiple_args: {file:"fun_mutiple_args", input: "", expected: "3628800"}
}

runtime_error_tests! {
    // integer_overflow_add1: { file: "integer_overflow_add1", input: "4611686018427387903", expected: "integer overflow" },
    // integer_overflow_sub1: { file: "integer_overflow_sub1", input: "-4611686018427387904", expected: "integer overflow" },
    // integer_overflow_mul: { file: "integer_overflow_mul", input: "2", expected: "integer overflow" },
    // integer_overflow_mul_negative: { file: "integer_overflow_mul_negative", input: "2", expected: "integer overflow" },
    // integer_overflow_plus: { file: "integer_overflow_plus", input: "", expected: "integer overflow" },
    // integer_overflow_minus: { file: "integer_overflow_minus", input: "", expected: "integer overflow" },
}

static_error_tests! {
    // Type checking error tests
    type_test_3_any_error: { file: "type_test_3_any_error", input: "", expected: "Expected Num type" },
    type_test_8_input_error: { file: "type_test_8_input_error", input: "", expected: "Expected Num type" },

    // test_loop_break_error: { file: "loop_break", input: "", expected: "break outside of loop" },
    // duplicate_function_name: { file: "duplicate_fun", input: "", expected: "Multiple functions are defined with the same name" },
    // undefined_function: { file: "undefined_fun", input: "", expected: "" },
    // input_in_function: { file: "input_in_fun", input: "5", expected: "Unbound variable identifier input" },
    // wrong_arg_count: { file: "wrong_arg_count", input: "", expected: "" },
}


repl_tests! {
    // test_simple_bools: ["(define x true)", "x", "false"] => ["true", "false"],

    // // Basic function definition and call
    // test_fun_simple: [
    //     "(fun (double x) (+ x x))",
    //     "(double 5)"
    // ] => ["10"],

    // // Function with multiple arguments
    // test_fun_multi_args: [
    //     "(fun (add3 x y z) (+ (+ x y) z))",
    //     "(add3 1 2 3)"
    // ] => ["6"],

    // // Recursive function
    // test_fun_recursive: [
    //     "(fun (fact n) (if (= n 0) 1 (* n (fact (sub1 n)))))",
    //     "(fact 5)"
    // ] => ["120"],

    // // Multiple functions calling each other
    // test_fun_chain: [
    //     "(fun (add2 x) (+ x 2))",
    //     "(fun (mul3 x) (* x 3))",
    //     "(mul3 (add2 5))"
    // ] => ["21"],

    // // Define variable and use it
    // test_define_use: [
    //     "(define x 42)",
    //     "(+ x 8)"
    // ] => ["50"],

    // // Multiple defines
    // test_define_multiple: [
    //     "(define a 10)",
    //     "(define b 20)",
    //     "(+ a b)"
    // ] => ["30"],

    // // Set! on defined variable
    // test_set_define: [
    //     "(define counter 0)",
    //     "counter",
    //     "(set! counter 10)",
    //     "counter"
    // ] => ["0", "10"],

    // // Set! multiple times
    // test_set_multiple: [
    //     "(define x 1)",
    //     "(set! x 2)",
    //     "(set! x (+ x 3))",
    //     "x"
    // ] => ["5"],

    // // Function using defined variable
    // test_fun_with_define: [
    //     "(define base 100)",
    //     "(fun (add_base x) (+ x base))",
    //     "(add_base 23)"
    // ] => ["123"],

    // // Function and set! on defined variable
    // test_fun_set_define: [
    //     "(define total 0)",
    //     "(fun (add_to_total x) (set! total (+ total x)))",
    //     "(add_to_total 5)",
    //     "total",
    //     "(add_to_total 10)",
    //     "total"
    // ] => ["5", "5", "15", "15"],

    // // Complex: function using and modifying defined variables
    // test_fun_modify_globals: [
    //     "(define x 10)",
    //     "(define y 20)",
    //     "(fun (swap) (let ((temp x)) (block (set! x y) (set! y temp) x)))",
    //     "(swap)",
    //     "x",
    //     "y"
    // ] => ["20", "20", "10"],

    // // Print in REPL
    // test_print_repl: [
    //     "(fun (test_print x) (block (print x) (+ x 1)))",
    //     "(test_print 5)"
    // ] => ["5\n6"],

    // // Loop with defined variable
    // test_loop_with_define: [
    //     "(define result 1)",
    //     "(loop (if (> result 10) (break result) (set! result (* result 2))))"
    // ] => ["16"],

    // // Function calling another function with defines
    // test_nested_fun_define: [
    //     "(define multiplier 3)",
    //     "(fun (triple x) (* x multiplier))",
    //     "(fun (triple_and_add y) (+ (triple y) 10))",
    //     "(triple_and_add 4)"
    // ] => ["22"],
}
