#lang rosette/safe

(provide (all-defined-out))

(struct op-rel (r xs) #:transparent)
(struct op (f xs) #:transparent)

(struct op-I (x) #:transparent)

(struct op-&& (x y) #:transparent)
(struct op-|| (x y) #:transparent)

(struct op-+ (x y) #:transparent)
(struct op-- (x y) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-/ (x y) #:transparent)
(struct op-inv (x) #:transparent)

(struct op-leq (x y) #:transparent)
(struct op-eq? (x y) #:transparent)

(struct op-sum (v e) #:transparent)
(struct op-exists (v e) #:transparent)

(define-symbolic inv (~> integer? integer?))
(define-symbolic sum (~> integer? integer? integer?))
(define-symbolic exist (~> integer? boolean? boolean?))

(define (I b) (if b 1 0))
(define (div x y) (* x (inv y)))
(define (rel r . xs) (apply r xs))

(define-symbolic temp integer?)
(assert (forall (list temp) (= (sum temp 0) 0)))
(assert (forall (list temp) (= (sum 0 temp) temp)))
