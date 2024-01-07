# Jenga

Jenga is a [stack-oriented](https://en.wikipedia.org/wiki/Stack-oriented_programming), [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) programming language, similar to [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)).

### Example

```
# The Collatz conjecture:
# f(n) = n / 2      if n is even
#      = 3 * n + 1  if n is odd

42
while dup 1 != do
    dup println
    if dup 2 % 0 = then
        2 /
    else
        3 * 1 +
    end
end
println
```

Output:

```
42
21
64
32
16
8
4
2
1
```

See more [examples](https://github.com/jdpmk/jenga/tree/master/examples).

### References

- [Stack-oriented programming](https://en.wikipedia.org/wiki/Stack-oriented_programming)
- [Forth programming language](https://en.wikipedia.org/wiki/Forth_(programming_language))
- [Porth programming language](https://gitlab.com/tsoding/porth)
- ["OCaml Programming: Correct + Efficient + Beautiful" (Cornell University: CS 3110)](https://github.com/cs3110/textbook)
- [Programming Languages and Compilers (University of Illinois Urbana-Champaign: CS 421)](https://courses.engr.illinois.edu/cs421/fa2022/)
