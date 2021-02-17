#lang rosette

(require "core/ops.rkt"
         "core/process.rkt"
         "core/interpret.rkt"
         "core/grammar.rkt")

(require rosette/lib/destruct
         rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

;; 1. declare relations and types

(struct rel-D (s t) #:transparent)
(define-symbolic D (~> integer? integer? integer?))

(struct rel-E (x y) #:transparent)
(define-symbolic E (~> integer? integer? boolean?))

(struct rel-sigma (s t) #:transparent)
(define-symbolic sigma (~> integer? integer? integer?))

;; 2. define macros TODO call interpret

(struct op-sigma (s v t) #:transparent)
(define (sigma-3 s v t)
             (op-* (op-* (rel-sigma s v) (rel-sigma v t))
                   (op-I-BN (op-eq? (rel-D s t)
                                    (op-+ (rel-D s v)
                                          (rel-D v t))))))

(struct op-delta (s v t) #:transparent)
(define (delta s v t)
               (op-* (op-sigma s v t) (op-inv (rel-sigma s t))))

;; 3. define types for variables

(define-symbolic s t u v integer?)

;; 4. map symbols to declared constructs

(define vars
  (list (cons 's s)
        (cons 't t)
        (cons 'u u)
        (cons 'v v)))

(define ops
  (list (cons 'E rel-E)
        (cons 'D rel-D)
        (cons 'sigma rel-sigma)))

;; 5. extend the interpreter over relations and macros
;; TODO might want to inline the interpreter

(define (interp-prog p)
  (define (interp p)
    (interpret interp-prog vars p))
  (destruct p
    [(rel-D x y) (D (interp x) (interp y))]
    [(rel-E x y) (E (interp x) (interp y))]
    [(rel-sigma x y) (sigma (interp x) (interp y))]
    [(op-sigma w x y) (interp (sigma-3 (interp w) (interp x) (interp y)))]
    [(op-delta w x y) (interp (delta (interp w) (interp x) (interp y)))]
    [p p]))

;; 6. ASSERTS
(define-symbolic x y z integer?)
(assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (D x y)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (sigma x y)))))

(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))

;; INPUT
(define prog
  (op-+ (op-sum-i-i 't (op-* (op-I-BN (rel-E 'v 't))
                             (op-delta 's 'v 't)))
        (op-sum-i-i 'u
                (op-sum-i-i 't
                        (op-/ (op-* (op-* (op-I-BN (rel-E 'v 't))
                                          (op-* (op-I-BN (op-eq? (rel-D 's 'u)
                                                              (op-+ (rel-D 's 'v)
                                                                    (rel-D 'v 'u))))
                                                (op-I-BN (op-eq? (rel-D 'v 'u)
                                                              (op-+ 1
                                                                    (rel-D 't 'u))))))
                                    (op-* (rel-sigma 's 'v) (rel-sigma 't 'u)))
                              (rel-sigma 's 'u))))))

;; GRAMMAR

;; all variables and ground terms of that type
(define (??v) (choose* 's 'u 'v 't))

;; +, * and additional semiring operations
(define (??op) (choose* op-+ op-* op-/))

;; terms
(define (??term depth)
  (gen-term depth (??v)
            (list (op-I-BN (rel-E (??v) (??v)))
                  (op-I-BN (rel-sigma (??v) (??v)))
                  (op-delta (??v) (??v) (??v)))
            ??op))

(define sketch
  (op-+ (op-sum-i-i 't
                    (op-* (op-delta 's 'v 't)
                          (??term 0)))
        (op-sum-i-i (??v)
                (op-sum-i-i 't
                        (op-* (op-delta 's 'v 't)
                              (??term 1))))))

(define M
  (synthesize
   #:forall (list E sigma D s t u v sum-i-i inv)
   #:guarantee (assert (eq? (interpret interp-prog vars sketch) (interpret interp-prog vars prog)))))

(evaluate sketch M)
