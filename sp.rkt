#lang rosette
(require "core/lib.rkt")

;; declare the types of relations and variables
(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

;; define helper functions
(def (weight w x z)
  ;; min w . 1_{E(x,z,w)} + w.
  (sum w (* (I (E x z w)) w)))

;; recursive stratum
(rec (f R)
  (λ (x z w) (+
     ;; R(x,z,w) :- E(x,z,w).
     (I (E x z w))
     ;; R(x,z,w) :- R(x,y,w1), E(y,x,w2), w=w1+w2.
     (sum y (sum w1 (sum w2
       (* (* (I (R x y w1)) (I (E y z w2))) (I (= w (* w1 w2))))))))))

;; "return" stratum
(ret (g R)
  ;; S[x,z] = min w . R(x,z,w) + w.
  (λ (x z) (sum w (* (R x z w) w))))

(optimize)

;; (+ (weight w x z) (* (S x y) (sum y (weight w2 y z))))
