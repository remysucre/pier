#lang rosette
(require "core/lib.rkt")

;; declare the types of relations and variables
(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

;; define helper functions
(def fun (weight w x z)
  (sum w (* w (I (E x z w)))))

;; normalized input program g.f
(define p
  (+ (weight w x z)
     (sum y
          (sum w1
               (* (weight w2 y z)
                  (* w1 (I (R x y w1))))))))

;; normalized g
(define g
  '(sum w1 (* w1 (I (R x y w1)))))

(optimize p g)
