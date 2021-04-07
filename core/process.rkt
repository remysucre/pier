#lang rosette

(require "ops.rkt")
(require rosette/lib/destruct)

(provide (all-defined-out))

;; sexp to struct
(define (preprocess e var rel fun)
  (define (prep e)
    (match e
      [(? symbol? e) (hash-ref var e e)]
      [`(I ,e) (op-I (prep e))]
      [`(inv ,e) (op-inv (prep e))]
      [`(* ,x ,y) (op-* (prep x) (prep y))]
      [`(+ ,x ,y) (op-+ (prep x) (prep y))]
      [`(- ,x ,y) (op-- (prep x) (prep y))]
      [`(= ,x ,y) (op-eq? (prep x) (prep y))]
      [`(<= ,x ,y) (op-leq (prep x) (prep y))]
      [`(sum ,y ,e) (op-sum (hash-ref var y) (prep e))]
      [`(,f ,vs ...) (cond [(hash-has-key? rel f) (op-rel (hash-ref rel f) (map prep vs))]
                           [else (op (hash-ref fun f) (map prep vs))])]
      [_ e]))
  (prep e))

;; struct to sexp
(define (postprocess e var rel fun)
  (define (post e)
    (match e
      [(op-I e) `(I ,(post e))]
      [(op-inv e) `(inv ,(post e))]
      [(op-* x y) `(* ,(post x) ,(post y))]
      [(op-+ x y) `(+ ,(post x) ,(post y))]
      [(op-- x y) `(- ,(post x) ,(post y))]
      [(op-eq? x y) `(= ,(post x) ,(post y))]
      [(op-leq x y) `(<= ,(post x) ,(post y))]
      [(op-sum v b) `(sum ,(post v) ,(post b))]
      [(op f es) `(,(hash-ref fun f) ,@(map post es))]
      [(op-rel r es)`(,(hash-ref rel r) ,@(map post es))]
      [(expression _ r es ...)`(,(hash-ref rel r) ,@(map post es))]
      [x (hash-ref var x x)]))
  (post e))
