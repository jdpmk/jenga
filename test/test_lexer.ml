open OUnit2
open Jenga.Lexer
open Jenga.Parser

let lex (s : string) : token list =
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
         ( "char with a single letter" >:: fun _ ->
           assert_equal [ CHAR 'a' ] (lex "'a'") );
         ( "char with a single digit" >:: fun _ ->
           assert_equal [ CHAR '0' ] (lex "'0'") );
         ( "string with a single word" >:: fun _ ->
           assert_equal [ STRING "jenga" ] (lex "\"jenga\"") );
         ( "string with multiple words" >:: fun _ ->
           assert_equal [ STRING "hello world" ] (lex "\"hello world\"") );
         ( "string with digits" >:: fun _ ->
           assert_equal [ STRING "testing 123" ] (lex "\"testing 123\"") );
         ("true" >:: fun _ -> assert_equal [ TRUE ] (lex "true"));
         ("false" >:: fun _ -> assert_equal [ FALSE ] (lex "false"));
         ( "identifiers" >:: fun _ ->
           assert_equal
             [ IDENTIFIER "x"; IDENTIFIER "jenga"; IDENTIFIER "abc123" ]
             (lex "x jenga abc123") );
         ("plus" >:: fun _ -> assert_equal [ PLUS ] (lex "+"));
         ("minus" >:: fun _ -> assert_equal [ MINUS ] (lex "-"));
         ("times" >:: fun _ -> assert_equal [ TIMES ] (lex "*"));
         ("divide" >:: fun _ -> assert_equal [ DIVIDE ] (lex "/"));
         ("exp" >:: fun _ -> assert_equal [ EXP ] (lex "**"));
         ("eq" >:: fun _ -> assert_equal [ EQ ] (lex "="));
         ("neq" >:: fun _ -> assert_equal [ NEQ ] (lex "!="));
         ("lt" >:: fun _ -> assert_equal [ LT ] (lex "<"));
         ("leq" >:: fun _ -> assert_equal [ LEQ ] (lex "<="));
         ("gt" >:: fun _ -> assert_equal [ GT ] (lex ">"));
         ("geq" >:: fun _ -> assert_equal [ GEQ ] (lex ">="));
         ("land" >:: fun _ -> assert_equal [ LAND ] (lex "&&"));
         ("lor" >:: fun _ -> assert_equal [ LOR ] (lex "||"));
         ("lnot" >:: fun _ -> assert_equal [ LNOT ] (lex "!"));
         ("print" >:: fun _ -> assert_equal [ PRINT ] (lex "print"));
         ("println" >:: fun _ -> assert_equal [ PRINTLN ] (lex "println"));
         ("eprint" >:: fun _ -> assert_equal [ EPRINT ] (lex "eprint"));
         ("eprintln" >:: fun _ -> assert_equal [ EPRINTLN ] (lex "eprintln"));
         ("read" >:: fun _ -> assert_equal [ READ ] (lex "->"));
         ("write" >:: fun _ -> assert_equal [ WRITE ] (lex "<-"));
         ( "ifelse" >:: fun _ ->
           assert_equal
             [ IF; INT 1; EQ; THEN; INT 1; PRINTLN; ELSE; INT 2; PRINTLN; END ]
             (lex "if 1 = then 1 println else 2 println end") );
         ( "while" >:: fun _ ->
           assert_equal
             [ WHILE; INT 1; EQ; DO; INT 1; PRINTLN; END ]
             (lex "while 1 = do 1 println end") );
         ( "alloc variable" >:: fun _ ->
           assert_equal
             [ ALLOC; IDENTIFIER "x"; AS; TINT; INT 0; END ]
             (lex "alloc x as int 0 end") );
         ( "alloc array" >:: fun _ ->
           assert_equal
             [
               ALLOC; IDENTIFIER "x"; AS; TBOOL; LBRACKET; RBRACKET; FALSE; END;
             ]
             (lex "alloc x as bool[] false end") );
         ("whitespace" >:: fun _ -> assert_equal [ TRUE ] (lex "  \ttrue\t  "));
         ("comment" >:: fun _ -> assert_equal [] (lex " # this is a comment \n"));
         ( "multiple tokens" >:: fun _ ->
           assert_equal [ INT 1; INT 2; PLUS; PRINTLN ] (lex "1 2 + println") );
       ]

let () = run_test_tt_main tests
