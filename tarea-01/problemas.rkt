#lang racket

(define (countdown n)
  (if(equal? n 0)
     (list 0)
     (cons n(countdown(- n 1))))
)

(define (insertL a b ls)
  (if(null? ls)
     ls
     (if (eqv? a (first ls))
         (cons b(cons a (insertL(a b (rest ls)))))
         (insertL(a b (rest ls)))))
  )

(define (remv-1st a ls)
  (if(null? ls)
     ls
     (if(eqv? a (first ls))
        (rest ls)
        (cons (first ls) (cons(remv-1st a (rest ls))))))
  )


(define (map sub1 ls)
  (if (null? ls)
      ls
      (cons (- (first ls) sub1) (cons (map sub1 (rest ls)))))
  )

(define (list-index-ofv c ls)
  (if (null? ls)
      ls
      (if (eqv? c (first ls))
          0
          (+ 1 (list-index-ofv c (rest ls)))))
  )

(define (append ls1 ls2)
  (if (null? ls2)
      ls1
      (append (cons ls1 (first ls2)) (rest ls2)))
  )

(define (repeat ls n)
  (if (zero? n)
      ls
      (repeat (cons ls ls) (- n 1)))
 )

(define (same-lists* ls1 ls2)
  (if (null? ls1)
      (if (null? ls2)
          #t
          #f)
      (if (eqv? (first ls1) (first ls2))
          (same-lists* (rest ls1) (rest ls2))
          #f))
  )

(define (bin->nat ls val count)
  (if (null? ls)
      val
      (bin->nat (rest ls)(+ val (*(first ls)count))(* 2 count))))

(define (binary->natural ls)
  (if(null? ls)
     0
     (if(eqv? 1 (firstls))
        (1+(bin->nat ls (rest ls) 1))
        (bin->nat ls (rest ls) 1))))


(provide (all-defined-out))
