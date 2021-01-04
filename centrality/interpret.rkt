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
    ;; RELATIONS
    [(rel-D x y)
     (D (interpret x) (interpret y))]
    [(rel-E x y)
     (E (interpret x) (interpret y))]
    ;; UDFs
    [(rel-sigma s t)
     (sigma (interpret s)
            (interpret t))]
    [(op-sigma s v t)
     (interpret (sigma-3 (interpret s)
                         (interpret v)
                         (interpret t)))]
    [(op-delta s v t)
     (interpret (delta (interpret s)
                       (interpret v)
                       (interpret t)))]
    ;; VARIABLES
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]
    ))

;; PER PROGRAM DEFINITIONS

;; STRUCTS

(struct rel-D (s t) #:transparent)
(struct rel-E (x y) #:transparent)
(struct rel-sigma (s t) #:transparent)
(struct op-sigma (s v t) #:transparent)
(struct op-delta (s v t) #:transparent)

;; RELATIONS

(define-symbolic D (~> integer? integer? integer?))
(define-symbolic E (~> integer? integer? boolean?))
(define-symbolic sigma (~> integer? integer? integer?))

;; VARIABLES

(define-symbolic s t u v integer?)

(define vars
  (list (cons 's s)
        (cons 't t)
        (cons 'u u)
        (cons 'v v)))

;; UDFs

(define (sigma-3 s v t)
  (op-* (op-* (rel-sigma s v) (rel-sigma v t))
        (op-I-BN (op-eq? (rel-D s t)
                      (op-+ (rel-D s v)
                            (rel-D v t))))))

(define (delta s v t)
  (op-* (op-sigma s v t) (op-inv (rel-sigma s t))))

;; ASSERTS
(define-symbolic x y z integer?)
(assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))
(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))
(assert (forall (list x y) (<=> (E x y) (= 1 (D x y)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (sigma x y)))))
