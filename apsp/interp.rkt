#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt")

(provide (all-defined-out))

(define (interpret p)
  (destruct p
    [(op-t* x y)
     (t* (interpret x )
         (interpret y))]
    [(op-t+ x y)
     (t+ (interpret x)
         (interpret y))]
    [(op-sum v e)
     (sig (interpret v)
          (interpret e))]
    [(op-sum-int v e)
     (sig-int (interpret v)
              (interpret e))]
    [(op-weight w x y)
     (weight (interpret w)
             (interpret x)
             (interpret y))]
    [(op-I e)
     (I (interpret e))]
    [(rel-E x y w)
     (E (interpret x) (interpret y) (interpret w))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))

(define (fvs p)
  (destruct p
    [(op-t* x y) (append (fvs x) (fvs y))]
    [(op-t+ x y) (append (fvs x) (fvs y))]
    [(op-eq? x y) (append (fvs x) (fvs y))]
    [(op-sum v e) (remove* (fvs v) (fvs e))]
    [(op-sum-int v e) (remove* (fvs v) (fvs e))]
    [(op-weight w x y) (list x y)]
    [(op-I e) (fvs e)]
    [(rel-E x y w) (append (fvs x) (fvs y) (fvs w))]
    [(rel-R x y w) (append (fvs x) (fvs y) (fvs w))]
    [_ (if (assoc p vars) (list p) '())]))

(assert (eq? (fvs 'x) '(x)))
(assert (eq? (fvs (rel-E 'x 'y 'w)) '(x y w)))
(assert (eq? (fvs (op-t* (rel-E 'x 'y 'w) (rel-E 'x 'y 'w)))
             '(x y w x y w)))
(assert (eq? (fvs (op-sum 'w (rel-E 'x 'y 'w))) '(x y)))
(assert (eq? (fvs (R-code 'x 'z 'w)) '(x z w x x w)))

(define (factors e)
  (destruct e
    [(op-t* x y) (append (factors x) (factors y))]
    [_ (list e)]))

(define (prod xs)
  (cond
    [(empty? xs) 0]
    [else (foldr (lambda (x p) (op-t* x p)) (car xs) (cdr xs))]))

;; push v into e
(define (push-var v e)
  (define-values (l r)
    (partition (lambda (f) (member v (fvs f))) (factors e)))
  (cons (prod l) (prod r)))

;; assumes normalized p
;; i.e. adding sums of products
(define (push-sum p)
  (destruct p
    [(op-t+ x y) (op-t+ (push-sum x) (push-sum y))]
    [(op-t* x y) (op-t* (push-sum x) (push-sum y))]
    [(op-sum v e)
     (define lr (push-var v (push-sum e)))
     (op-t* (op-sum v (car lr)) (cdr lr))]
    [(op-sum-int v e)
     (define lr (push-var v (push-sum e)))
     (op-t* (op-sum-int v (car lr)) (cdr lr))]
    [_ p]))

;; (op-sum 'w (op-t* (op-I (op-eq? 'w x)) 'w))
;; x
(define (simplify p)
  (destruct p
    [(op-sum v e)
     (if (eq? p (op-sum 'w (op-t* (op-I (op-eq? 'w (op-t* 'w1 'w2))) 'w)))
         (op-t* 'w1 'w2)
         (op-sum v (simplify e)))]
    [(op-sum-int v e) (op-sum-int v (simplify e))]
    [(op-t+ x y) (op-t+ (simplify x) (simplify y))]
    [(op-t* x y) (op-t* (simplify x) (simplify y))]
    [_ p]))
