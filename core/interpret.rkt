#lang rosette/safe

(require rosette/lib/destruct)
(require "ops.rkt")

(provide (all-defined-out))

;; COMMON DEFINITIONS
(define (interpret vars rels ops p)
  (define (interp p)
  (destruct p
    [(op-I e) (I (interp e))]
    [(op-&& x y) (&& (interp x) (interp y))]
    [(op-|| x y) (|| (interp x) (interp y))]
    [(op-+ x y) (+ (interp x) (interp y))]
    [(op-- x y) (- (interp x) (interp y))]
    [(op-* x y) (* (interp x) (interp y))]
    [(op-/ x y) (* (interp x) (interp (op-inv (interp y))))]
    [(op-inv x) (inv (interp x))]
    [(op-leq x y) (<= (interp x) (interp y))]
    [(op-eq? x y) (eq? (interp x) (interp y))]
    [(op-sum v e) (sum (interp v) (interp e))]
    [(op-exists v e) (exist (interp v) (interp e))]
    ;; relations
    [(op-rel r xs) (apply r (map interp xs))]
    ;; UDF
    [(op f xs) (interp (apply f xs))]
    ;; variables and constants
    [p p]))
  (interp p))
