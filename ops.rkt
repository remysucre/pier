#lang rosette/safe

(provide (all-defined-out))

(struct op-I-BN (x) #:transparent)
(struct op-I-BT (x) #:transparent)

(struct op-&& (x y) #:transparent)
(struct op-|| (x y) #:transparent)

(struct op-+ (x y) #:transparent)
(struct op-- (x y) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-/ (x y) #:transparent)
(struct op-inv (x) #:transparent)

(struct op-leq (x y) #:transparent)
(struct op-eq? (x y) #:transparent)

(struct op-t+ (x y) #:transparent)
(struct op-t* (x y) #:transparent)

(struct op-sum-i-i (v e) #:transparent)
(struct op-sum-i-t (v e) #:transparent)
(struct op-sum-t-t (v e) #:transparent)
(struct op-exists (v e) #:transparent)

(define-symbolic inv (~> integer? integer?))
(define-symbolic sum-i-i (~> integer? integer? integer?))
(define-symbolic exist (~> integer? boolean? boolean?))

;; a tropical number is either a real or #f (for inf)
(define inf #f)
(define inf? boolean?)

;; #t => 0, #f => infty
(define (I-BT b) (if b 0 inf))
(define (I-BN b) (if b 1 0))

(define (t+ a b)
  (if (inf? a)
      b
      (if (inf? b)
          a
          (min a b))))

(define (t* a b)
  (if (inf? a)
      inf
      (if (inf? b)
          inf
          (+ a b))))

;; uninterpreted tropical summation

(define-symbolic sum-t-inf-inf-r real?)
(define-symbolic sum-t-inf-inf-b boolean?)
(define sum-t-inf-inf (if sum-t-inf-inf-b inf sum-t-inf-inf-r))

(define-symbolic sum-t-inf-r-r (~> real? real?))
(define-symbolic sum-t-inf-r-b (~> real? boolean?))
(define (sum-t-inf-r x)
  (if (sum-t-inf-r-b x) inf (sum-t-inf-r-r x)))

(define-symbolic sum-t-r-inf-r (~> real? real?))
(define-symbolic sum-t-r-inf-b (~> real? boolean?))
(define (sum-t-r-inf x)
  (if (sum-t-r-inf-b x) inf (sum-t-r-inf-r x)))

(define-symbolic sum-t-r-r-r (~> real? real? real?))
(define-symbolic sum-t-r-r-b (~> real? real? boolean?))
(define (sum-t-r-r x y)
  (if (sum-t-r-r-b x y) inf (sum-t-r-r-r x y)))

(define (sum-t-t v e)
  (if (inf? v)
      (if (inf? e)
          sum-t-inf-inf
          (sum-t-inf-r e))
      (if (inf? e)
          (sum-t-r-inf v)
          (sum-t-r-r v e))))

;; summation over integer domain

(define-symbolic sum-t-i-inf-b (~> integer? boolean?))
(define-symbolic sum-t-i-inf-i (~> integer? real?))
(define (sum-t-i-inf v)
  (if (sum-t-i-inf-b v) inf (sum-t-i-inf-i v)))

(define-symbolic sum-t-i-i-b (~> integer? real? boolean?))
(define-symbolic sum-t-i-i-i (~> integer? real? real?))
(define (sum-t-i-i v e)
  (if (sum-t-i-i-b v e) inf (sum-t-i-i-i v e)))

(define (sum-i-t v e)
  (if (inf? e)
      (sum-t-i-inf v)
      (sum-t-i-i v e)))
