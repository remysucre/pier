#lang rosette/safe

(require rosette/lib/synthax)

(define-symbolic x integer?)

(define-synthax (maths x depth)
  #:base (choose x 2)
  #:else (choose
          x 2
          ((choose + *) (maths x (- depth 1))
                        (maths x (- depth 1)))))


(define (equ x)
  (maths x 3))

(define O
  (synthesize
   #:forall (list x)
   #:guarantee (assert (= (equ x) (* 2 x)))))

(print-forms O)
