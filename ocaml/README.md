# zlang: The completely laZy language

For my custom final project for Harvard CS 51, with the permission of Dr. Chong, I implemented my own language to experiment with combination of semantics that no existing programming language uses together:

 - All functions are pure
 - Each function is its own type
 - Expressions are always lazy, and only forced at the top level
 - Functions can be overloaded, and the appropriate dispatch is selected during lazy expression forcing
 - Strict superset of MiniML (my extensions include novel lazy multiple-dispatch, Lisp syntax, many more types, and a strongly-typed language)

## Syntax

The syntax is Lisp-like. Specifically, it follows the LL(7) grammar of Scheme Lisp as described in the R<sup>5</sup>RS. I wrote a recursive descent parser for this grammar from scratch in `expr.ml`. The scoping is dynamic.

## Examples and tests
zlang is already powerful enough that I wrote tests in zlang itself (tests.zl). See below, with additional commentary:

```scheme
;; "If" and other constructs are no longer primitives;
;; they can be constructed naturally using zlang's unique lazy dispatch:
(define `(if #t ,then ,else) then)
(define `(if #f ,then ,else) else)

(if (= 1 1)
    "passed"
    "FAILED")

;; An elegant unit testing framework can be made similarly:
(define `(unit-test #t) "passed")
(define `(unit-test #f) "FAILED")

;; Test that functions are first-class citizens in zlang:
(define 'lambda-function
  (function `,x
	    (+ x 1)))
(unit-test (= 2 (lambda-function 1)))

;; Test arithmetic:
(unit-test (= (* 2 2) (+ 2 2)))
(unit-test (= (- 1) -1))
(unit-test (< 1. 1.01))

;; Test scoping (dynamic);
(begin
  (define 'x 1)
  (unit-test (= x 1)))
(begin
  (define 'x 2)
  (unit-test (= x 2)))
```

We see that it is not even necessary to define `if` as a primitive of the language when using zlang’s lazy multiple-dispatch. In fact, the only primitives are `define`, `function`.

A REPL is also included, and it can be accessed by running `./zlang.byte` directly.

## Future directions

Currently, dispatches and environments are stored as association lists, in the Lisp tradition, but these can be made into OCaml `Hashtbl`s for performance.
