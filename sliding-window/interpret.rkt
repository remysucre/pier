
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
    ;;

    [(op-vec-get j w t)
     (interpret
     (vec-get (interpret j)
              (interpret w)
              (interpret t)))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [(rel-v i x)
     (v (interpret i) (interpret x))]
    [(rel-S t)
     (interpret (S (interpret t)))]
    [(rel-window t)
     (interpret (window (interpret t)))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]
    ))


(struct op-vec-get (j w t) #:transparent)
(struct rel-v (i v) #:transparent)
(struct rel-R (x y w) #:transparent)
(struct rel-S (t) #:transparent)
(struct rel-window (t) #:transparent)

(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))
(define-symbolic t j k integer?)
(define-symbolic w integer?)

(define (vec-get j w t)
  (op-sum-i-i j
              (op-sum-i-i w
                      (op-* w
                            (op-* (op-* (op-I-BN (op-eq? j t))
                                        (op-I-BN (op-leq 1 t)))
                                  (op-I-BN (rel-v j w)))))))

(define (S t)
  (op-sum-i-i 'j
              (op-sum-i-i 'w
                      (op-* (op-* 'w (op-I-BN (rel-R t 'j 'w)))
                            (op-* (op-I-BN (op-leq 1 'j))
                                  (op-I-BN (op-leq 'j t)))))))

(define (window t)
  (op-- (S (op-- t 1)) (S (op-- (op-- t 'k) 1))))

(define vars
  (list (cons 't t)
        (cons 'j j)
        (cons 'k k)
        (cons 'w w)))
