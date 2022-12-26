# Jenga

Jenga is a [stack-oriented](https://en.wikipedia.org/wiki/Stack-oriented_programming), [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) programming language, similar to [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)).

## Examples

```
# A comment begins with the `#` character

# Push the string literal "Hello, World!", then print it out

"Hello, World!" println

# Push integers `1` and `2` and operator `+`, then print the result

1 2 + println

# `-2` parsed as the integer negation operator applied to `2`, while `-` is parsed as the integer subtraction operator

1 -2 - println

# if statement
# ... if [condition] then [if body] else [else body] end

if 1 1 = then
    1 println
else
    2 println
end

# while loop
# ... while [condition] do [body] end

0 while dup 5 =/= do
    dup print
    " " print
    1 +
end drop
"" println
```

## Language

Stack manipulation operators:
- `push ( -- a )` (implicit) - push a literal or operator onto the stack
- `dup ( a -- a a )` - duplicate the item at the top of the stack
- `drop ( a -- )` - remove the item at the top of the stack
- `swap ( a b -- b a )` - swap the topmost and second topmost items on the stack
- `over ( a b -- a b a )` - duplicate the second topmost item on the stack
- `rot ( a b c -- b c a )` - rotate the top three items on the stack

Literals:
- `..., -2, -1, 0, 1, 2, ...` - integers
- `"", "jenga", "jenga\n", ...` - strings
- `false, true` - booleans

Arithmetic operators:
- `+` - addition
- `-` - subtraction
- `*` - multiplication
- `/` - division
- `%` - modulus
- `**` - exponentiation

Comparison operators:
- `=` - equality
- `=/=` - not equal to
- `<` - less than
- `>` - greater than
- `<=` - less than or equal to
- `>=` - greater than or equal to

Logical operators:
- `&&` - and
- `||` - or
- `!` - not

I/O functions:
- `print` - print to stdout
- `println` - print to stdout, followed by a newline (`"\n"`)
- `eprint` - print to stderr
- `eprintln` - print to stderr, followed by a newline (`"\n"`)

Control flow:
- `if [condition] then [if body] else [else body] end` - if statement
- `while [condition] do [body] end` - while loop

## Appendix

References:
- [Stack-oriented programming](https://en.wikipedia.org/wiki/Stack-oriented_programming)
- [Forth programming language](https://en.wikipedia.org/wiki/Forth_(programming_language))
- [Porth programming language](https://gitlab.com/tsoding/porth)
- ["OCaml Programming: Correct + Efficient + Beautiful" (Cornell University: CS 3110)](https://github.com/cs3110/textbook)
- [Programming Languages and Compilers (University of Illinois Urbana-Champaign: CS 421)](https://courses.engr.illinois.edu/cs421/fa2022/)
