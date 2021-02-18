#lang rosette

(require "core/ops.rkt" "core/interpret.rkt" "core/grammar.rkt" "core/macro.rkt")
(require rosette/lib/angelic) ;; provide choose*

(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

(def fun (weight w x z)
  (op-sum w (op-* w (op-I (op-rel E (list x z w))))))

;; INPUT

(define prog
  (op-+ (op weight (list w x z))
        (op-sum y
         (op-sum w1
          (op-* (op weight (list w2 y z))
                (op-* w1
                      (op-I (op-rel R (list x y w1)))))))))

;; GRAMMAR

;; all variables and ground terms of that type
(define (??v) (choose* x y z))
(define (??w) (choose* w w1 w2))

;; +, * and additional semiring operations
(define (??op) (choose* op-+ op-*))

;; TODO rename this to factor
(define (??term depth)
  (gen-term depth (??w)
            (list (op-I (op-rel E (list (??v) (??v) (??w))))
                  (op-I (op-rel R (list (??v) (??v) (??w))))
                  (op weight (list (??w) (??v) (??v))))
            ??op))

;; factors also include aggregates
(define (??factor depth)
  (gen-factor depth (list (??v) (??w)) op-sum ??term ??op))

;; additional layers of aggregates
(define (??agg depth e)
  (gen-agg depth e (list (??v) (??w)) op-sum))

;; defined from normalized G
(define sketch
  (op-+ (??factor 0)
        (??agg 1
               (op-sum w1
                           (??agg 0
                                  (op-* (op-* (op-I (op-rel R (list x y w1))) w1)
                                        (??term 0)))))))

(define (interp p) (interpret (unbox var) (unbox rel) (unbox fun) p))

(define M
  (synthesize
   #:forall (list R E x y z w w1 w2 sum)
   #:guarantee (assert (eq? (interp sketch) (interp prog)))))

(evaluate sketch M)
