#lang plait

(define-type <expr>
  (num [n : Number])
  (sec [e1 : <expr>]
       [e2 : <expr>]))

(define-type S-Expr 
  (<num> [n : Number])
  (<string> [s : Symbol])
  (<identifier> [id : Symbol])
  (true [b : Boolean])
  (false [b : Boolean])
  (+ [n1 : <expr>]
     [n2 : <expr>])
  (++ [s1 : <expr>]
      [s2 : <expr>])
  (num= [n1 : <expr>]
        [n2 : <expr>])
  (str= [s1 : <expr>]
        [s2 : <expr>])
  (if [a : <expr>]
      [b : <expr>]
      [c : <expr>])
  (and [e1 : <expr>]
       [e2 : <expr>])
  (or [e1 : <expr>]
      [e2 : <expr>])
  (let [id : S-Expr]
    [e1 : <expr>]
    [e2 : <expr>])
  (fun [id : S-Expr]
       [e : <expr>]))

(define-type ExprS
  (define-type binopS
    (sum [e1 : S-Expr]
         [e2 : S-Expr])
    (concat [e1 : S-Expr]
            [e2 : S-Expr])))

(define (interp [in : ExprC]) : Value)

(define (parse-number [in : S-Expr]) : ExprS
  (<num> in))

(define (parse-string [in : S-Expr]) : ExprS
  (<string> in))

(define (boolS [in : S-Expr]) : ExprS
  (true in))

(define (parse-if [in : S-Expr]) : ExprS
  (if (parse(first in))(parse(second in)(parse(third in)))))

(define (parse-and [in : S-Expr]) : ExprS
  (and (parse (first in))(parse(second in))))

(define (parse-or [in : S-Expr]) : ExprS
  (or (parse (first in))(parse(second in))))

(define (parse-+ [in : S-Expr]) : ExprS
  (+ (parse (first in))(parse(second in))))

(define (parse-++ [in : S-Expr]) : ExprS
  (++ (parse (first in))(parse(second in))))

(define (parse-num= [in : S-Expr]) : ExprS
  (num= (parse(first in))(parse(second in))))

(define (parse-str= [in : S-Expr]) : ExprS
  (str= (parse(first in))(parse(second in))))

(define (parse-fun [in : S-Expr]) : ExprS
  (fun (parse(first in))(parse(second in))))

(define (parse-let [in : S-Expr]) : ExprS
  (let (parse(first in))(parse(second in))))

(define (parse-app [in : S-Expr]) : ExprS
  ;No sé si este retorna el error, o no.
  (++ (parse(first in))(parse(second in))))

(define (parse-id [in : S-Expr]) : ExprS
  (<identifier> in))

;Definicion que no debe ser modificada
(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in) (parse-number in)]
    [(s-exp-string? in) (parse-string in)]
    [(s-exp-match? `true in) (boolS #t)]
    [(s-exp-match? `false in) (boolS #f)]
    [(s-exp-match? `{if ANY ...} in) (parse-if in)]
    [(s-exp-match? `{and ANY ...} in) (parse-and in)]
    [(s-exp-match? `{or ANY ...} in) (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in) (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in) (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in) (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in) (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in) (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in) (parse-app in)]
    [(s-exp-symbol? in) (parse-id in)]))
;Definición que no debe ser modificada
(define (eval [str : S-Expr]) : Value
  (interp(desugar(parse str))))