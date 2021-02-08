
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
    [(op-lt x y)
     (< (interpret x)
        (interpret y))]
    [(op-gt x y)
     (> (interpret x)
        (interpret y))]
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
    ;;

    [(rel-G x y)
     (G (interpret x) (interpret y))]
    [(rel-P i x y)
     (P (interpret i) (interpret x) (interpret y))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]
    ))

(struct op-lt (x y) #:transparent)
(struct op-gt (x y) #:transparent)

(struct rel-G (i v) #:transparent)
(struct rel-P (x y w) #:transparent)

(define-symbolic P (~> integer? integer? integer? boolean?))
(define-symbolic G (~> integer? integer? boolean?))
(define-symbolic t x y p integer?)

(define vars
  (list (cons 't t)
        (cons 'x x)
        (cons 'y y)
        (cons 'p p)))
