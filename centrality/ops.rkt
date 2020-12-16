#lang rosette/safe

(provide (all-defined-out))

(struct op-sum (v e) #:transparent)
(struct op-+ (x y) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-/ (x y) #:transparent)
(struct op-inv (x) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct op-I (x) #:transparent)
(struct rel-D (s t) #:transparent)
(struct rel-sigma (s t) #:transparent)
(struct op-sigma (s v t) #:transparent)
(struct op-delta (s v t) #:transparent)
(struct rel-E (x y) #:transparent)
