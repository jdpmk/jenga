open OUnit2
open Jenga.Env

let tests =
  "env tests"
  >::: [
         ( "get" >:: fun _ ->
           assert_equal (Some 1) (get [ ("a", 1); ("b", 2) ] "a") );
         ( "put new" >:: fun _ ->
           assert_equal
             [ ("c", 3); ("b", 2); ("a", 1) ]
             (put [ ("a", 1); ("b", 2) ] "c" 3) );
         ( "put replace" >:: fun _ ->
           assert_equal
             [ ("c", 4); ("b", 2); ("a", 1) ]
             (put [ ("a", 1); ("c", 3); ("b", 2) ] "c" 4) );
       ]

let () = run_test_tt_main tests
