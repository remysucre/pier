#lang rosette

(require "core/ops.rkt"
         "core/process.rkt"
         "core/interpret.rkt"
         "core/grammar.rkt")

(require rosette/lib/destruct
         rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

;; 1. declare relations and types

(struct rel-E (x y z) #:transparent)
(define-symbolic E (~> integer? integer? integer? boolean?))

(struct rel-R (x y z) #:transparent)
(define-symbolic R (~> integer? integer? integer? boolean?))

;; 2. define macros

(struct op-weight (w x y) #:transparent)
(define (weight w x z)
  (interpret interp-prog vars
             (op-sum-i-i w (op-* w (op-I-BN (rel-E x z w))))))

;; 3. define types for variables

(define-symbolic x y z integer?)
(define-symbolic w w1 w2 integer?)

;; 4. map symbols to declared constructs

(define vars
  (list (cons 'x x) (cons 'y y) (cons 'z z)
        (cons 'w w) (cons 'w1 w1) (cons 'w2 w2)))

(define ops
  (list (cons 'weight op-weight)
        (cons 'R rel-R)
        (cons 'E rel-E)))

;; 5. extend the interpreter over relations and macros

(define (interp-prog p)
  (define (interp p)
    (interpret interp-prog vars p))
  (destruct p
    [(op-weight w x y) (weight (interp w) (interp x) (interp y))]
    [(rel-E x y w) (E (interp x) (interp y) (interp w))]
    [(rel-R x y w) (R (interp x) (interp y) (interp w))]
    [p p]))

;; INPUT

(define prog
  (op-+ (op-weight 'w 'x 'z)
        (op-sum-i-i 'y (op-sum-i-i 'w1 (op-* (op-weight 'w2 'y 'z) (op-* 'w1 (op-I-BN (rel-R 'x 'y 'w1))))))))

;; GRAMMAR

;; all variables and ground terms of that type
(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))

;; +, * and additional semiring operations
(define (??op) (choose* op-+ op-*))

;; TODO rename this to factor
(define (??term depth)
  (gen-term depth (??w)
            (list (op-I-BN (rel-E (??v) (??v) (??w)))
                  (op-I-BN (rel-R (??v) (??v) (??w)))
                  (op-weight (??w) (??v) (??v)))
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
                                  (op-* (op-* (op-I-BN (rel-R 'x 'y 'w1)) 'w1)
                                        (??term 0)))))))

(define M
  (synthesize
   #:forall (list R E x y z w w1 w2 sum-i-i)
   #:guarantee (assert (eq? (interpret interp-prog vars sketch) (interpret interp-prog vars prog)))))

(evaluate sketch M)
