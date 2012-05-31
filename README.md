scalisp
=======

Simple lisp repl, a project for a Scala Breakfast exercise at Gilt Ireland.

Supports just a tiny subset of some micro lisp: defun, if, +, -, *, /, <, >, <=, >= =.

Example:

    $ sbt run
    [info] Loading global plugins from /Users/ebowman/.sbt/plugins
    [info] Set current project to scalisp (in build file:/Users/ebowman/src/scalisp/)
    [info] Running Driver
    > (defun fac (x) "" (if (< x 2) x (* x (fac (- x 1)))))
    (51 ms) defined Var(fac)
    > fac(10)
    [1.1] failure: `(' expected but `f' found

    fac(10)
    ^
    > (fac 10)
    (6 ms) 3628800
    > (defun high (x y f) "" (f x y))
    (2 ms) defined Var(high)
    > (high 2 3 (lamba (x y) (+ x y)))
    (2 ms) 5

