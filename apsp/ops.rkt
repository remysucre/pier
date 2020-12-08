#lang rosette/safe

(provide (all-defined-out))

(struct op-weight (x y) #:transparent)
(struct op-sum (v e) #:transparent)
(struct op-sum-int (v e) #:transparent)
(struct op-t+ (x y) #:transparent)
(struct op-t* (x y) #:transparent)
(struct op-I (e) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct rel-E (x y w) #:transparent)
(struct rel-R (x y w) #:transparent)

;; #t => 0, #f => infty
(define (I b) (if b 0 #t))

(define (t+ a b)
  (if (boolean? a)
      b
      (if (boolean? b)
          a
          (min a b))))

(define (t* a b)
  (if (boolean? a)
      #t
      (if (boolean? b)
          #t
          (+ a b))))

;; uninterpreted tropical summation

(define-symbolic sig-ii-n real?)
(define-symbolic sig-ii-b boolean?)
(define sig-ii (if sig-ii-b #t sig-ii-n))

(define-symbolic sig-in-n (~> real? real?))
(define-symbolic sig-in-b (~> real? boolean?))
(define (sig-in x)
  (if (sig-in-b x) #t (sig-in-n x)))

(define-symbolic sig-ni-n (~> real? real?))
(define-symbolic sig-ni-b (~> real? boolean?))
(define (sig-ni x)
  (if (sig-ni-b x) #t (sig-ni-n x)))

(define-symbolic sig-nn-n (~> real? real? real?))
(define-symbolic sig-nn-b (~> real? real? boolean?))
(define (sig-nn x y)
  (if (sig-nn-b x y) #t (sig-nn-n x)))

(define (sig v e)
  (if (boolean? v)
      (if (boolean? e)
          sig-ii
          (sig-in e))
      (if (boolean? e)
          (sig-ni v)
          (sig-nn v e))))

;; summation over integer domain

(define-symbolic sig-int-i-b (~> integer? boolean?))
(define-symbolic sig-int-i-n (~> integer? real?))
(define (sig-int-i v)
  (if (sig-int-i-b v) #t (sig-int-i-n v)))

(define-symbolic sig-int-n-b (~> integer? real? boolean?))
(define-symbolic sig-int-n-n (~> integer? real? real?))
(define (sig-int-n v e)
  (if (sig-int-n-b v e) #t (sig-int-n-n v e)))

(define (sig-int v e)
  (if (boolean? e)
      (sig-int-i v)
      (sig-int-n v e)))
