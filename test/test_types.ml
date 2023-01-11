open OUnit2
open Jenga.Ast
open Jenga.Exceptions
open Jenga.Lexer
open Jenga.Parser
open Jenga.Types

let lex_and_parse_and_type_check (s : string) =
  let lexbuf = Lexing.from_string s in
  let program = parse_program read_token lexbuf in
  type_check_program program

let tests =
  "types tests"
  >::: [
         ( "push int" >:: fun _ ->
           assert_equal [ TInt ] (type_check_command [] (Value (Int 1))) );
         ( "push string" >:: fun _ ->
           assert_equal [ TString ]
             (type_check_command [] (Value (String "jenga"))) );
         ( "push bool" >:: fun _ ->
           assert_equal [ TBool ] (type_check_command [] (Value (Bool false)))
         );
         ( "arithmetic operator" >:: fun _ ->
           assert_equal () (lex_and_parse_and_type_check "1 2 + println") );
         ( "stack manipulation" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check "1 2 3 rot drop drop drop") );
         ( "arithmetic and logical operators" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check "4 4 * 2 5 ** 16 - = true && println")
         );
         ( "simple ifelse" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check
                "if 1 1 = then 1 println else 2 println end") );
         ( "simple while" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check
                "0 while dup 5 != do dup print \" \" print 1 + end drop \"\" \
                 println") );
         ( "not enough items" >:: fun _ ->
           assert_raises
             (TypeError
                "cannot execute `+`. expected two items of type `int` on the \
                 stack but found one item or none") (fun _ ->
               lex_and_parse_and_type_check "+") );
         ( "type mismatch" >:: fun _ ->
           assert_raises
             (TypeError
                "cannot execute `-`. expected `int` and `int` but found `int` \
                 and `bool`") (fun _ ->
               lex_and_parse_and_type_check "1 true - println") );
         ( "ifelse condition mismatch" >:: fun _ ->
           assert_raises (TypeError "if-else condition must produce a `bool`")
             (fun _ ->
               lex_and_parse_and_type_check
                 "if 1 then 1 println else 2 println end") );
         ( "invalid ifelse body" >:: fun _ ->
           assert_raises
             (TypeError "if body must not modify the structure of the stack")
             (fun _ ->
               lex_and_parse_and_type_check
                 "if true then 1 println 1 else 2 println 1 end") );
         ( "while condition mismatch" >:: fun _ ->
           assert_raises
             (TypeError "while loop condition must produce a `bool`") (fun _ ->
               lex_and_parse_and_type_check "while 1 do 1 println end") );
         ( "invalid while body" >:: fun _ ->
           assert_raises
             (TypeError
                "while loop body must not modify the structure of the stack")
             (fun _ -> lex_and_parse_and_type_check "while true do 1 end") );
       ]

let () = run_test_tt_main tests
