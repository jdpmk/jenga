open OUnit2

let tests = "parser tests" >::: []
let () = run_test_tt_main tests
