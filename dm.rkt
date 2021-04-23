#lang rosette

;; T(x,y,w) :- E(x,y,w).
;; T(x,y,w) :- E(x,z,w1), T(z,y,w2), w=w1+w2.

;; Given: E . y -> x, w
;; Prove: T . x, y -> w

(define-symbolic T (~> integer? integer? integer? boolean?))
(define-symbolic E (~> integer? integer? integer? boolean?))
(define-symbolic x y z w w1 w2 integer?)

;; E . y -> x, w
(assert (forall (list x y z w1 w2)
                (=> (&& (E x z w1) (E y z w2))
                    (&& (= x y) (= w1 w2)))))

;; needs induction!
;; T . x, y -> w
(assert (forall (list x y w1 w2)
                (=> (&& (T x y w1) (T x y w2))
                    (= w1 w2))))

(define (ef x y w)
  (&& (T x z w1)
      (E z y w2)
      (= w (+ w1 w2))))

(verify (assert (=> (&& (ef x y w1) (ef x y w2))
                    (= w1 w2))))
