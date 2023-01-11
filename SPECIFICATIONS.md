# Specifications

This document describes the specifications of the Jenga programming language,
including language semantics, syntactic constructs, and intrinsic functions and
operations.

## Language

Stack manipulation operators:
- `dup ( a -- a a )` - duplicate the item at the top of the stack
- `drop ( a -- )` - remove the item at the top of the stack
- `swap ( a b -- b a )` - swap the topmost and second topmost items on the stack
- `over ( a b -- a b a )` - duplicate the second topmost item on the stack
- `rot ( a b c -- b c a )` - rotate the top three items on the stack

Literals:
- `..., -2, -1, 0, 1, 2, ...` - integers
- `"", "jenga", "jenga\n", ...` - strings
- `false`, `true` - booleans

Arithmetic operators:
- `+` - addition
- `-` - subtraction
- `*` - multiplication
- `/` - division
- `%` - modulus
- `**` - exponentiation

Comparison operators:
- `=` - equality
- `!=` - not equal to
- `<` - less than
- `<=` - less than or equal to
- `>` - greater than
- `>=` - greater than or equal to

Logical operators:
- `&&` - logical and
- `||` - logical or
- `!` - logical not

I/O functions:
- `print` - print to stdout
- `println` - print to stdout, followed by a newline (`"\n"`)
- `eprint` - print to stderr
- `eprintln` - print to stderr, followed by a newline (`"\n"`)

Control flow:
- `if [condition] then [if body] else [else body] end` - if statement
- `while [condition] do [body] end` - while loop
