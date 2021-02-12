#lang rosette/safe

(require rosette/lib/destruct)
(require "../ops.rkt")

(provide (all-defined-out))

;; COMMON DEFINITIONS

(define (interpret p)
  (destruct p
    [(op-I-BN e)
     (I-BN (interpret e))]
    [(op-I-BT e)
     (I-BT (interpret e))]
    [(op-&& x y)
     (&& (interpret x)
         (interpret y))]
    [(op-|| x y)
     (|| (interpret x)
         (interpret y))]
    [(op-+ x y)
     (+ (interpret x)
        (interpret y))]
    [(op-- x y)
     (- (interpret x)
        (interpret y))]
    [(op-* x y)
     (* (interpret x)
        (interpret y))]
    [(op-/ x y)
     (* (interpret x)
        (interpret (op-inv (interpret y))))]
    [(op-inv x)
     (inv (interpret x))]
    [(op-leq x y)
     (<= (interpret x)
         (interpret y))]
    [(op-eq? x y)
     (eq? (interpret x)
          (interpret y))]
    [(op-t* x y)
     (t* (interpret x )
         (interpret y))]
    [(op-t+ x y)
     (t+ (interpret x)
         (interpret y))]
    [(op-sum-i-i v e)
     (sum-i-i (interpret v)
              (interpret e))]
    [(op-sum-i-t v e)
     (sum-i-t (interpret v)
              (interpret e))]
    [(op-sum-t-t v e)
     (sum-t-t (interpret v)
              (interpret e))]
    [(op-exists v e)
     (exist (interpret v)
            (interpret e))]
    ;; PER PROGRAM DEFS
    [(op-weight w x y)
     (interpret (weight (interpret w)
                        (interpret x)
                        (interpret y)))]
    [(rel-E x y w)
     (E (interpret x) (interpret y) (interpret w))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))

;; PER PROGRAM DEFINITIONS

;; STRUCTS

(struct op-weight (w x y) #:transparent)
(struct rel-E (w x y) #:transparent)
(struct rel-R (w x y) #:transparent)

;; RELATIONS

(define-symbolic E-i-n (~> integer? integer? boolean?))
(define-symbolic E-n-n (~> integer? integer? real? boolean?))
(define (E x y w) (if (inf? w) (E-i-n x y) (E-n-n x y w)))

(define-symbolic R-i-n (~> integer? integer? boolean?))
(define-symbolic R-n-n (~> integer? integer? real? boolean?))
(define (R x y w) (if (inf? w) (R-i-n x y) (R-n-n x y w)))

;; UDFs

(define (weight w x z)
  (op-sum-t-t w
              (op-t* w (op-I-BT (rel-E x z w)))))

;; VARIABLES

(define-symbolic x y z integer?)
(define-symbolic w-i w1-i w2-i boolean?)
(define-symbolic w-n w1-n w2-n real?)
(define w (if w-i inf w-n))
(define w1 (if w1-i inf w1-n))
(define w2 (if w2-i inf w2-n))

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'w w)
        (cons 'w1 w1)
        (cons 'w2 w2)))
