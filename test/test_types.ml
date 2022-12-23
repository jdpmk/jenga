open OUnit2
open Jenga.Ast
open Jenga.Exceptions
open Jenga.Lexer
open Jenga.Parser
open Jenga.Types

let lex_and_parse_and_type_check (s : string) =
  let lexbuf = Lexing.from_string s in
  let program = parse_program read_token lexbuf in
  type_check program

let tests =
  "types tests"
  >::: [
         ( "push int" >:: fun _ ->
           assert_equal [ TInt ] (type_check_token [] (Const (Int 1))) );
         ( "push string" >:: fun _ ->
           assert_equal [ TString ]
             (type_check_token [] (Const (String "jenga"))) );
         ( "push bool" >:: fun _ ->
           assert_equal [ TBool ] (type_check_token [] (Const (Bool false))) );
         ( "good program 1" >:: fun _ ->
           assert_equal () (lex_and_parse_and_type_check "1 2 + println") );
         ( "good program 2" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check "1 2 3 rot drop drop drop") );
         ( "good program 3" >:: fun _ ->
           assert_equal ()
             (lex_and_parse_and_type_check "4 4 * 2 5 ** 16 - = true && println")
         );
         ( "bad program 1" >:: fun _ ->
           assert_raises
             (TypeError
                "cannot execute `+`. expected two items of type `int` on the \
                 stack but found one item or none") (fun _ ->
               lex_and_parse_and_type_check "+") );
         ( "bad program 2" >:: fun _ ->
           assert_raises
             (TypeError
                "cannot execute `-`. expected `int` and `int` but found `int` \
                 and `bool`") (fun _ ->
               lex_and_parse_and_type_check "1 true - println") );
       ]

let () = run_test_tt_main tests
