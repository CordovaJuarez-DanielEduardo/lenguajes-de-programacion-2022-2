#lang plait

(define-type ArithC
  (numC [a : Number])
  (plusC [a : ArithC]
        [b : ArithC])
  (restC [a : ArithC]
        [b : ArithC])
  (multC [a : ArithC]
        [b : ArithC])
  (negC [a : ArithC])
  (bool [true : Boolean]
        [false : Boolean])
  (or [a : ArithC]
        [b : ArithC])
  (and [a : ArithC]
        [b : ArithC])
  (not [a : ArithC])
  (less [a : ArithC]
        [b : ArithC])
  (great [a : ArithC]
        [b : ArithC])
  (equal [a : ArithC]
        [b : ArithC]))

(define (s-expr-number? s)
  (if (numC? s)
      #t
      #f))

;(provide (all-defined-out))
