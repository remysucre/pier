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

;; #t => 0, #f => infty
(define (I b) (if b 1 0))
