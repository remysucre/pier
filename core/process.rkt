#lang rosette

(require "ops.rkt")

(provide (all-defined-out))

(define (preprocess get-def e)
  (match e
    [(? symbol? e) e]
    [`(I ,e) (op-I (preprocess get-def e))]
    [`(inv ,e) (op-inv (preprocess get-def e))]
    [`(* ,x ,y) (op-* (preprocess get-def x) (preprocess get-def y))]
    [`(+ ,x ,y) (op-+ (preprocess get-def x) (preprocess get-def y))]
    [`(sum ,y ,e) (op-sum y (preprocess get-def e))]
    [`(,f ,vs ...) (apply (get-def f) (map (curry preprocess get-def) vs))]))

(define (pr p var rel)
  (define (proc p)
    (match p
      [`(sum ,v ,e) (op-sum (proc v) (proc e))]
      [`(* ,x ,y) (op-* (proc x) (proc y))]
      [`(I ,e) (op-I (proc e))]
      [`(,r ,xs ...) (op-rel (hash-ref rel r) (map proc xs))]
      [x (hash-ref var x)]))
  (proc p))

(define (show p ler nuf rav)
  (define (show p)
  (match p
    [(op-+ x y) `(+ ,(show x) ,(show y))]
    [(op f xs) (cons (hash-ref nuf f) (map show xs))]
    [(op-sum v e) `(sum ,(hash-ref rav v) ,(show e))]
    [(op-* x y) `(* ,(show x) ,(show y))]
    [(op-I e) `(I ,(show e))]
    [(op-rel r xs) (cons (hash-ref ler r) (map show xs))]
    [x (hash-ref rav x)]))
  (show p))
