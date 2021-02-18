#lang rosette

(require "core/ops.rkt" "core/interpret.rkt" "core/grammar.rkt" "core/macro.rkt")
(require rosette/lib/angelic) ;; provide choose*

(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

(def fun (weight w x z) (sum w (* w (I (E x z w)))))

;; INPUT

(define p
  (+ (weight w x z)
     (sum y
          (sum w1
               (* (weight w2 y z)
                  (* w1 (I (R x y w1))))))))


(define sketch (gen-grammar var-type rel-type fun-type (list op-+ op-*) var rel fun))

(define M
  (synthesize
   #:forall (list R E x y z w w1 w2 sum)
   #:guarantee (assert (eq? (interpret sketch) p))))

(evaluate sketch M)
