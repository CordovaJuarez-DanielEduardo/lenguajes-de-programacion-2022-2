#lang plait

(define-type ArithC
  (numC [a : Number])
  (plusC [a : ArithC]
        [b : ArithC])
  (multC [a : ArithC]
        [b : ArithC])
  (negC [a : ArithC])
  ;(bool [true : Boolean]
  ;      [false : Boolean])
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

;Se requiere validar el tipo de expresión correspondiente al AritC
(define (s-expr-number? s)
  (if (numC? s)
      #t
      #f))
(define (s-expr-plus? s)
  (if (plusC? s)
      #t
      #f))
(define (s-expr-mul? s)
  (if (multC? s)
      #t
      #f))
(define (s-expr-neg? s)
  (if (negC? s)
      #t
      #f))
(define (s-expr-or? s)
  (if (or? s)
      #t
      #f))
(define (s-expr-and? s)
  (if (and? s)
      #t
      #f))
(define (s-expr-less? s)
  (if (less? s)
      #t
      #f))
(define (s-expr-great? s)
  (if (great? s)
      #t
      #f))
(define (s-expr-equal? s)
  (if (equal? s)
      #t
      #f))


(define (parse [s : S-Exp]) : AritC
  (cond [(s-exp-number? s) (numC (s-exp->number))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(+) (plusC (parse (second ls)) (parse (third ls)))]
             [(*) (multC (parse (second ls)) (parse (third ls)))]
             [(-) (negC (0) (parse (second ls)))]
             [(&) (andC (parse (second ls)) (parse (third ls)))]
             [(?) (orC (parse (second ls)) (parse (third ls)))]
             [(<) (lessC (parse (second ls)) (parse (third ls)))]
             [else (error 'parse "operacion aritmetica malformada")]))]
        [else (error 'parse "operacion aritmetica malformada")]))

(define (desugar [s : S-Exp] : S-Exp)
  (cond [(s-exp-number? s) (numC (s-exp->number))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             ;[(+) (plusC (parse (second ls)) (parse (third ls)))]
             [(-) (restS (plusC (parse (second ls)) (negC (parse (third ls)))))]
             ;[(*) (multC (parse (second ls)) (parse (third ls)))]
             ;[(-) (negC (0) (parse (second ls)))]
             ;[(&) (andC (parse (second ls)) (parse (third ls)))]
             ;[(?) (orC (parse (second ls)) (parse (third ls)))]
             ;[(<) (lessC (parse (second ls)) (parse (third ls)))]
             [(>) (greatS (lessC (parse (third ls)) (parse (scond ls)))]
             [else (error 'desugar "operacion aritmetica malformada")]))]
        [else (error 'desugar "operacion aritmetica malformada")]))

(define (interp [s : S-Exp] : S-Exp))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

;(provide (all-defined-out))
