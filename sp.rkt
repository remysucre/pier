#lang rosette

(require "core/ops.rkt" "core/interpret.rkt" "core/grammar.rkt" "core/macro.rkt")
(require rosette/lib/angelic) ;; provide choose*

(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

(def fun (weight w x z) (sum w (* w (I (E x z w)))))

rel-type

;; INPUT

(define p
  (+ (weight w x z)
     (sum y
          (sum w1
               (* (weight w2 y z)
                  (* w1 (I (R x y w1))))))))

;; GRAMMAR

;; ;; all variables and ground terms of that type
;; (define (??v t) (apply choose* (hash-ref var-type t)))

;; ;; +, * and additional semiring operations
;; (define (??op) (choose* op-+ op-*))

;; ;; TODO rename this to factor
;; (define (??term depth)
;;   (gen-term depth (??v 'int?)
;;             (list (op-I (op-rel (choose* E R) (list (??v 'id?) (??v 'id?) (??v 'int?))))
;;                   (op weight (list (??v 'int?) (??v 'id?) (??v 'id?))))
;;             ??op))

;; ;; factors also include aggregates
;; (define (??factor depth)
;;   (gen-factor depth (list (??v 'id?) (??v 'int?)) op-sum ??term ??op))

;; ;; additional layers of aggregates
;; (define (??agg depth e)
;;   (gen-agg depth e (list (??v 'id?) (??v 'int?)) op-sum))

;; ;; defined from normalized G
;; (define sketch
;;   (op-+ (??factor 0)
;;         (??agg 1
;;                (op-sum w1
;;                        (??agg 0
;;                               (op-* (op-* (op-I (op-rel R (list x y w1))) w1)
;;                                     (??term 0)))))))

(define sketch (gen-grammar var-type rel-type (list op-+ op-*) var rel fun))

(define M
  (synthesize
   #:forall (list R E x y z w w1 w2 sum)
   #:guarantee (assert (eq? (interpret sketch) p))))

(evaluate sketch M)
