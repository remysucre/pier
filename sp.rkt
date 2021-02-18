#lang rosette

(require "core/ops.rkt" "core/process.rkt" "core/interpret.rkt" "core/grammar.rkt" "core/macro.rkt")
(require rosette/lib/angelic) ;; provide choose*

;; 1. declare relations and types
(decl rel E (~> integer? integer? integer? boolean?))
(decl rel R (~> integer? integer? integer? boolean?))

;; 2. define macros
(def fun (weight w x z)
  (op-sum-i-i w (op-* w (op-I-BN (op-rel 'E (list x z w))))))

;; 3. define types for variables
(decl var x integer?)
(decl var y integer?)
(decl var z integer?)

(decl var w integer?)
(decl var w1 integer?)
(decl var w2 integer?)

;; INPUT

(define prog
  (op-+ (op 'weight '(w x z))
        (op-sum-i-i 'y
         (op-sum-i-i 'w1
          (op-* (op 'weight '(w2 y z))
                (op-* 'w1
                      (op-I-BN (op-rel 'R '(x y w1)))))))))

;; GRAMMAR

;; all variables and ground terms of that type
(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))

;; +, * and additional semiring operations
(define (??op) (choose* op-+ op-*))

;; TODO rename this to factor
(define (??term depth)
  (gen-term depth (??w)
            (list (op-I-BN (op-rel 'E (list (??v) (??v) (??w))))
                  (op-I-BN (op-rel 'R (list (??v) (??v) (??w))))
                  (op 'weight (list (??w) (??v) (??v))))
            ??op))

;; factors also include aggregates
(define (??factor depth)
  (gen-factor depth (list (??v) (??w)) op-sum-i-i ??term ??op))

;; additional layers of aggregates
(define (??agg depth e)
  (gen-agg depth e (list (??v) (??w)) op-sum-i-i))

;; defined from normalized G
(define sketch
  (op-+ (??factor 0)
        (??agg 1
               (op-sum-i-i 'w1
                           (??agg 0
                                  (op-* (op-* (op-I-BN (op-rel 'R '(x y w1))) 'w1)
                                        (??term 0)))))))

(define (interp p) (interpret (unbox var) (unbox rel) (unbox fun) p))

(define M
  (synthesize
   #:forall (list R E x y z w w1 w2 sum-i-i)
   #:guarantee (assert (eq? (interp sketch) (interp prog)))))

(evaluate sketch M)
