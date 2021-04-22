#lang rosette

(define-symbolic E (~> integer? integer? integer?))
(define-symbolic x y z w integer?)

(define-symbolic uf (~> integer? integer?))
(define-symbolic sum-uf (~> integer? integer? integer?))

(define (sum x f) (sum-uf x (f x)))
(define (exist x f) (sum-uf x (* (f x) (f (uf x)))))

(assert (forall (list x y z w) (= (* (E x z) (E y z)) (E x z))))

(verify (assert (= (sum z (λ (z) (E z y)))
                   (exist z (λ (z) (E z y))))))
