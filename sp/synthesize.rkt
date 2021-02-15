#lang rosette

(require "../ops.rkt"
         "../process.rkt"
         "../interpret.rkt")

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

#;(preprocess get-def '(+
  (weight w x z)
  (sum-i-t y
    (sum-t-t w1
      (* (weight w2 y z)
        (* w1
          (I (R x y w1))))))))

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

(define-symbolic E-i-n (~> integer? integer? boolean?))
(define-symbolic E-n-n (~> integer? integer? real? boolean?))
(define (E x y w) (if (inf? w) (E-i-n x y) (E-n-n x y w)))

(define-symbolic R-i-n (~> integer? integer? boolean?))
(define-symbolic R-n-n (~> integer? integer? real? boolean?))
(define (R x y w) (if (inf? w) (R-i-n x y) (R-n-n x y w)))

;; 5. define macros

(define (weight w x z)
  (op-sum-t-t w
              (op-t* w (op-I-BT (rel-E x z w)))))

;; 6. define types for variables

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

;; INPUT

(define prog
  (op-t+
   (op-weight 'w 'x 'z)
   (op-sum-i-t 'y (op-sum-t-t 'w1 (op-t* (op-weight 'w2 'y 'z) (op-t* 'w1 (op-I-BT (rel-R 'x 'y 'w1))))))))

;; GRAMMAR

(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))
(define (??op) (choose* op-t+ op-t*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I-BT (rel-E (??v) (??v) (??w)))
               (op-I-BT (rel-R (??v) (??v) (??w)))
               (op-weight (??w) (??v) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum-t-t (??w) (??term (- depth 1)))
               (op-sum-i-t (??v) (??term (- depth 1))))))

(define (??agg depth e)
  (if (= depth 0)
      e
      (choose* (op-sum-i-t (??v) (??agg (- depth 1) e))
               (op-sum-t-t (??w) (??agg (- depth 1) e)))))

(define sketch
  (op-t+ (??term 0)
         (??agg 1
                (op-sum-t-t 'w1
                        (??agg 0
                        (op-t* (op-t* (op-I-BT (rel-R 'x 'y 'w1)) 'w1)
                               (??term 0)))))))

#;(define M
  (synthesize
   #:forall (list R-i-n R-n-n E-i-n E-n-n
                  x y z
                  w-i w1-i w2-i w-n w1-n w2-n
                  sum-t-inf-inf-r sum-t-inf-inf-b sum-t-inf-r-r sum-t-inf-r-b sum-t-r-inf-r sum-t-r-inf-b sum-t-r-r-r sum-t-r-r-b
                  sum-t-i-inf-b sum-t-i-inf-i sum-t-i-i-b sum-t-i-i-i)
   #:guarantee (assert (eq? (interpret interp-prog vars sketch) (interpret interp-prog vars prog)))))

;;(evaluate sketch M)
