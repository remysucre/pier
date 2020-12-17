#lang rosette/safe

(provide (all-defined-out))

(struct op-exists (v e) #:transparent)
(struct op-+ (x y) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-= (x y) #:transparent)
(struct rel-E (x y) #:transparent)
(struct rel-R (x y) #:transparent)
(struct rel-S (t) #:transparent)
(struct rel-T (z) #:transparent)

(define-symbolic sig (~> integer? boolean? boolean?))
