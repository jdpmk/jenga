open OUnit2
open Jenga.Ast
open Jenga.Lexer
open Jenga.Parser

let lex_and_parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = parse_program read_token lexbuf in
  ast

let tests =
  "parser tests"
  >::: [
         ( "empty program" >:: fun _ ->
           assert_equal (Program (Memory [], Block [])) (lex_and_parse "") );
         ( "single token" >:: fun _ ->
           assert_equal
             (Program (Memory [], Block [ UnaryOp Dup ]))
             (lex_and_parse "dup") );
         ( "mix of tokens" >:: fun _ ->
           assert_equal
             (Program
                ( Memory [],
                  Block
                    [
                      UnaryOp Dup;
                      UnaryOp Drop;
                      UnaryOp Swap;
                      Value (Primitive (Int 1));
                      Value (Primitive (Char 'a'));
                      Value (Primitive (String "a"));
                      Value (Primitive (Bool false));
                      Value (Primitive (Identifier "x"));
                    ] ))
             (lex_and_parse "dup drop swap 1 'a' \"a\" false x") );
         ( "simple program with a comment" >:: fun _ ->
           assert_equal
             (Program
                ( Memory [],
                  Block
                    [
                      Value (Primitive (Int 1));
                      Value (Primitive (Int 2));
                      BinaryOp Plus;
                      UnaryOp Println;
                    ] ))
             (lex_and_parse "# this is a comment\n1 2 + println") );
         ( "program with ifelse" >:: fun _ ->
           assert_equal
             (Program
                ( Memory [],
                  Block
                    [
                      Value (Primitive (Int 0));
                      IfElse
                        ( Block
                            [
                              UnaryOp Dup;
                              Value (Primitive (Int 1));
                              BinaryOp Eq;
                            ],
                          Block
                            [
                              Value (Primitive (String "equal"));
                              UnaryOp Println;
                            ],
                          Block
                            [
                              Value (Primitive (String "not equal"));
                              UnaryOp Println;
                            ] );
                    ] ))
             (lex_and_parse
                "0 if dup 1 = then \"equal\" println else \"not equal\" \
                 println end") );
         ( "program with while" >:: fun _ ->
           assert_equal
             (Program
                ( Memory [],
                  Block
                    [
                      Value (Primitive (Int 0));
                      While
                        ( Block
                            [
                              UnaryOp Dup;
                              Value (Primitive (Int 5));
                              BinaryOp Neq;
                            ],
                          Block
                            [
                              UnaryOp Dup;
                              UnaryOp Print;
                              Value (Primitive (String " "));
                              UnaryOp Print;
                              Value (Primitive (Int 1));
                              BinaryOp Plus;
                            ] );
                      Value (Primitive (String "\n"));
                      UnaryOp Print;
                    ] ))
             (lex_and_parse
                "0 while dup 5 != do dup print \" \" print 1 + end \"\n\" print")
         );
         ( "simple program with memory" >:: fun _ ->
           assert_equal
             (Program
                ( Memory
                    [
                      Alloc ("x", TPrimitive TInt, Primitive (Int 0));
                      Alloc ("xs", TCompound (TArr TInt), Primitive (Int 0));
                    ],
                  Block
                    [
                      Value (Primitive (Int 1));
                      Value (Primitive (Int 2));
                      BinaryOp Plus;
                      UnaryOp Println;
                    ] ))
             (lex_and_parse
                "alloc x as int 0 end alloc xs as int[] 0 end 1 2 + println") );
       ]

let () = run_test_tt_main tests
