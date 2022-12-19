open OUnit2
open Jenga.Lexer
open Jenga.Parser

let lex (s : string) =
  let lexbuf = Lexing.from_string s in
  let rec read_all_tokens lexbuf =
    match read_token lexbuf with
    | EOF -> []
    | token -> token :: read_all_tokens lexbuf
  in
  read_all_tokens lexbuf

let tests =
  "lexer tests"
  >::: [
         ("dup" >:: fun _ -> assert_equal [ DUP ] (lex "dup"));
         ("drop" >:: fun _ -> assert_equal [ DROP ] (lex "drop"));
         ("swap" >:: fun _ -> assert_equal [ SWAP ] (lex "swap"));
         ("over" >:: fun _ -> assert_equal [ OVER ] (lex "over"));
         ("rot" >:: fun _ -> assert_equal [ ROT ] (lex "rot"));
         ( "negative integer" >:: fun _ ->
           assert_equal [ INT (-123) ] (lex "-123") );
         ("zero" >:: fun _ -> assert_equal [ INT 0 ] (lex "0"));
         ("positive integer" >:: fun _ -> assert_equal [ INT 123 ] (lex "123"));
         ( "string with a single word" >:: fun _ ->
           assert_equal [ STRING "jenga" ] (lex "\"jenga\"") );
         ( "string with multiple words" >:: fun _ ->
           assert_equal [ STRING "hello world" ] (lex "\"hello world\"") );
         ( "string with digits" >:: fun _ ->
           assert_equal [ STRING "testing 123" ] (lex "\"testing 123\"") );
         ("true" >:: fun _ -> assert_equal [ TRUE ] (lex "true"));
         ("false" >:: fun _ -> assert_equal [ FALSE ] (lex "false"));
         ("plus" >:: fun _ -> assert_equal [ PLUS ] (lex "+"));
         ("minus" >:: fun _ -> assert_equal [ MINUS ] (lex "-"));
         ("times" >:: fun _ -> assert_equal [ TIMES ] (lex "*"));
         ("divide" >:: fun _ -> assert_equal [ DIVIDE ] (lex "/"));
         ("exp" >:: fun _ -> assert_equal [ EXP ] (lex "**"));
         ("eq" >:: fun _ -> assert_equal [ EQ ] (lex "="));
         ("neq" >:: fun _ -> assert_equal [ NEQ ] (lex "=/="));
         ("lt" >:: fun _ -> assert_equal [ LT ] (lex "<"));
         ("leq" >:: fun _ -> assert_equal [ LEQ ] (lex "<="));
         ("gt" >:: fun _ -> assert_equal [ GT ] (lex ">"));
         ("geq" >:: fun _ -> assert_equal [ GEQ ] (lex ">="));
         ("land" >:: fun _ -> assert_equal [ LAND ] (lex "&&"));
         ("lor" >:: fun _ -> assert_equal [ LOR ] (lex "||"));
         ("lnot" >:: fun _ -> assert_equal [ LNOT ] (lex "!"));
         ("print" >:: fun _ -> assert_equal [ PRINT ] (lex "print"));
         ("println" >:: fun _ -> assert_equal [ PRINTLN ] (lex "println"));
         ("whitespace" >:: fun _ -> assert_equal [ TRUE ] (lex "  \ttrue\t  "));
         ( "multiple tokens" >:: fun _ ->
           assert_equal [ INT 1; INT 2; PLUS; PRINTLN ] (lex "1 2 + println") );
       ]

let _ = run_test_tt_main tests
