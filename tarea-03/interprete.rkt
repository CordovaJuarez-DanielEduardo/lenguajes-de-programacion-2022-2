#lang plait

(define-type <expr>
  (num [n : Number])
  (sec [e1 : <expr>]
       [e2 : <expr>]))

(define-type S-Expr 
  (<num> [n : Number])
  (<string> [s : Symbol])
  (<identifier> [id : Symbol])
  (inlst [ls : list])
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
  (if [inlst : list])
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
  (numS [n : Number])
  (strS [str : Symbol])
  (idS [id : Symbol])
  (boolS [b : Boolean])
  (binopS [op : Symbol]
          [e1 : ExprS]
          [e2 : ExprS])
  (ifS [b : Boolean]
       [s1 : ExprS]
       [s2 : ExprS]
       [s3 : ExprS])
  (andS [e1 : ExprS]
        [e2 : Exprs])
  (orS [e1 : ExprS]
        [e2 : Exprs]))

;(define (interp [in : ExprC]) : Value)

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
(define (parse-number in)
  (numS (s-exp->number in)))
(define (parse-string in)
  (strS (s-exp->string in)))
(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de
1! funciones"))))

;Definición que no debe ser modificada
(define (eval [str : S-Expr]) : Value
  (interp(desugar(parse str))))