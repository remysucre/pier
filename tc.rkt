#lang rosette
(require "core/lib.rkt")

(decl rel E R (~> id? id? bool?))
(decl var x y z id?)

(define (r x y) `(I (R ,x ,y)))
(hash-set! meta 'r r)

(rec (f R)
     (λ (x y)
       (+ (I (E x y))
          (sum z (* (R x z)
                    (I (E z y)))))))

(ret (g R)
     (λ (y) (R 1 y)))

(hash-update! type->var 'id? (curry cons 1))

(optimize)

;; (+ (I (E 1 y)) (sum z (* (S z) (I (E z y)))))
