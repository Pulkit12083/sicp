#lang scheme
#|
Alyssa's code will run out of memory.
new-if is a function not a special form. Consequently,
when it's called in sqr-iter each of the parameters passed
to the new-if function are evaluated which causes the program
to always evaluate (improve guess x) before evaluating the
conditional clause. Infinite loop.

(new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))

|#
