scalalisp
========

Simple lisp interpreter for a Scala Breakfast exercise at Gilt Ireland.

Example:

    object Driver extends App {
      println(Lisp.parse( """(defun fib (n) "recursive" (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"""))
      println(Lisp.parse("(fib 40)"))
    }