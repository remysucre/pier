#lang rosette/safe

(require rosette/lib/destruct)
(require "ops.rkt")

(provide (all-defined-out))

;; COMMON DEFINITIONS
(define (interpret interp-prog vars p)
  (define (interp p)
  (destruct p
    [(op-I-BN e) (I-BN (interp e))]
    [(op-I-BT e) (I-BT (interp e))]
    [(op-&& x y) (&& (interp x) (interp y))]
    [(op-|| x y) (|| (interp x) (interp y))]
    [(op-+ x y) (+ (interp x) (interp y))]
    [(op-- x y) (- (interp x) (interp y))]
    [(op-* x y) (* (interp x) (interp y))]
    [(op-/ x y) (* (interp x) (interp (op-inv (interp y))))]
    [(op-inv x) (inv (interp x))]
    [(op-leq x y) (<= (interp x) (interp y))]
    [(op-eq? x y) (eq? (interp x) (interp y))]
    [(op-t* x y) (t* (interp x ) (interp y))]
    [(op-t+ x y) (t+ (interp x) (interp y))]
    [(op-sum-i-i v e) (sum-i-i (interp v) (interp e))]
    [(op-sum-i-t v e) (sum-i-t (interp v) (interp e))]
    [(op-sum-t-t v e) (sum-t-t (interp v) (interp e))]
    [(op-exists v e) (exist (interp v) (interp e))]
    [_ (define result (assoc p vars))
       (if result (cdr result) (interp-prog p))]))
  (interp p))
