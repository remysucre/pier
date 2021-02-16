#lang rosette

(require "ops.rkt"
         "process.rkt"
         "interpret.rkt")

(require rosette/lib/destruct
         rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

;; 1. declare a struct per relation & macro

(struct rel-E (x y z) #:transparent)
(struct rel-R (x y z) #:transparent)
(struct op-weight (w x y) #:transparent)

;; 2. specify how the structs map to symbols

(define (get-def f)
  (match f
    ['weight op-weight]
    ['R rel-R]
    ['E rel-E]))

;; 3. extend the interpreter over relations and macros

(define (interp-prog p)
  (define (interp p)
    (interpret interp-prog vars p))
  (destruct p
    [(op-weight w x y) (interp (weight (interp w) (interp x) (interp y)))]
    [(rel-E x y w) (E (interp x) (interp y) (interp w))]
    [(rel-R x y w) (R (interp x) (interp y) (interp w))]
    [p p]))

;; 4. declare symbolic types for the relations

(define-symbolic E (~> integer? integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

;; 5. define macros

(define (weight w x z)
  (op-sum-i-i w (op-* w (op-I-BN (rel-E x z w)))))

;; 6. define types for variables

(define-symbolic x y z integer?)
(define-symbolic w w1 w2 integer?)

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'w w)
        (cons 'w1 w1)
        (cons 'w2 w2)))

;; INPUT

(define prog
  (op-+ (op-weight 'w 'x 'z)
        (op-sum-i-i 'y (op-sum-i-i 'w1 (op-* (op-weight 'w2 'y 'z) (op-* 'w1 (op-I-BN (rel-R 'x 'y 'w1))))))))

;; GRAMMAR

(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))
(define (??op) (choose* op-+ op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I-BN (rel-E (??v) (??v) (??w)))
               (op-I-BN (rel-R (??v) (??v) (??w)))
               (op-weight (??w) (??v) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               #;(op-sum-t-t (??w) (??term (- depth 1)))
               (op-sum-i-i (??v) (??term (- depth 1))))))

(define (??agg depth e)
  (if (= depth 0)
      e
      (choose*
        (op-sum-i-i (??v) (??agg (- depth 1) e))
        (op-sum-i-i (??w) (??agg (- depth 1) e)))))

(define sketch
  (op-+ (??term 0)
     (??agg 1
            (op-sum-i-i 'w1
                        (??agg 0
                        (op-* (op-* (op-I-BN (rel-R 'x 'y 'w1)) 'w1)
                               (??term 0)))))))

(define opt
  (op-+ (op-weight 'w 'x 'z) (op-sum-i-i 'y (op-sum-i-i 'w1 (op-* (op-* (op-I-BN (rel-R 'x 'y 'w1)) 'w1) (op-weight 'w2 'y 'z))))))


(verify (assert (= (interpret interp-prog vars opt) (interpret interp-prog vars prog))))

(define M
  (synthesize
   #:forall (list R E
                  x y z
                  w w1 w2
                  sum-i-i)
   #:guarantee (assert (eq? (interpret interp-prog vars sketch) (interpret interp-prog vars prog)))))

(evaluate sketch M)
