scalalisp
========

Simple lisp interpreter for a Scala Breakfast exercise at Gilt Ireland.

Example:

    object Driver extends App {
      println(Lisp.parse( """(defun fib (n) "recursive" (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"""))
      println(Lisp.parse("(fib 40)"))
    }

Output:

    $ sbt run
    [info] Loading global plugins from /Users/ebowman/.sbt/plugins
    [info] Set current project to scalisp (in build file:/Users/ebowman/src/scalisp/)
    [info] Running Driver 
    defined Var(fib)
    102334155
    [success] Total time: 1 s, completed May 29, 2012 4:40:13 PM
