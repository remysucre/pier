#lang rosette

(require "core/ops.rkt"
         "core/process.rkt"
         "core/interpret.rkt"
         "core/grammar.rkt")

(require rosette/lib/destruct
         rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

;; 1. declare relations and types
(define-symbolic D (~> integer? integer? integer?))
(define-symbolic E (~> integer? integer? boolean?))
(define-symbolic sigma (~> integer? integer? integer?))

;; 2. define macros
(define (sigma-3 s v t)
  (op-* (op-* (rel 'sigma (list s v))
              (rel 'sigma (list v t)))
        (op-I-BN (op-eq? (rel 'D (list s t))
                         (op-+ (rel 'D (list s v))
                               (rel 'D (list v t)))))))

(define (delta s v t)
  (op-* (op 'sigma (list s v t))
        (op-inv (rel 'sigma (list s t)))))

;; 3. define types for variables

(define-symbolic s t u v integer?)

;; 4. map symbols to declared constructs

(define vars
  (list (cons 's s)
        (cons 't t)
        (cons 'u u)
        (cons 'v v)))

(define rels
  (list (cons 'E E)
        (cons 'D D)
        (cons 'sigma sigma)))

(define ops
  (list (cons 'sigma sigma-3)
        (cons 'delta delta)))

;; 6. ASSERTS
(define-symbolic x y z integer?)
(assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (D x y)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (sigma x y)))))

(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))

;; INPUT
(define prog
  (op-+ (op-sum-i-i 't (op-* (op-I-BN (rel 'E '(v t)))
                             (op 'delta '(s v t))))
        (op-sum-i-i 'u
                (op-sum-i-i 't
                        (op-/ (op-* (op-* (op-I-BN (rel 'E '(v t)))
                                          (op-* (op-I-BN (op-eq? (rel 'D '(s u))
                                                              (op-+ (rel 'D '(s v))
                                                                    (rel 'D '(v u)))))
                                                (op-I-BN (op-eq? (rel 'D '(v u))
                                                              (op-+ 1
                                                                    (rel 'D '(t u)))))))
                                    (op-* (rel 'sigma '(s v)) (rel 'sigma '(t u))))
                              (rel 'sigma '(s u)))))))

;; GRAMMAR

;; all variables and ground terms of that type
(define (??v) (choose* 's 'u 'v 't))

;; +, * and additional semiring operations
(define (??op) (choose* op-+ op-* op-/))

;; terms
(define (??term depth)
  (gen-term depth (??v)
            (list (op-I-BN (rel (choose* 'E 'sigma) (list (??v) (??v))))
                  (op 'delta (list (??v) (??v) (??v))))
            ??op))

(define sketch
  (op-+ (op-sum-i-i 't
                    (op-* (op 'delta '(s v t))
                          (??term 0)))
        (op-sum-i-i (??v)
                (op-sum-i-i 't
                        (op-* (op 'delta '(s v t))
                              (??term 1))))))

(define (interp p) (interpret vars rels ops p))
(define p (interp prog))

(define M
  (synthesize
   #:forall (list E sigma D s t u v sum-i-i inv)
   #:guarantee (assert (eq? (interp sketch) p))))

(evaluate sketch M)
