#lang rosette/safe

(provide (all-defined-out))

(struct op-vec-get (j w t) #:transparent)
(struct op-sum (v e) #:transparent)
(struct op-sum-int (v e) #:transparent)
(struct op-+ (x y) #:transparent)
(struct op-- (x y) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-I (e) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct op-leq (x y) #:transparent)
(struct rel-v (i v) #:transparent)
(struct rel-R (x y w) #:transparent)
(struct rel-S (t) #:transparent)

(define (I b) (if b 1 0))

(define-symbolic sig (~> integer? integer? integer?))
(define-symbolic sig-int (~> integer? integer? integer?))
