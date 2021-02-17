#lang rosette

(require "ops.rkt")

(provide (all-defined-out))

(define (preprocess get-def e)
  (match e
    [(? symbol? e) e]
    [`(I ,e) (op-I-BT (preprocess get-def e))]
    [`(inv ,e) (op-inv (preprocess get-def e))]
    [`(* ,x ,y) (op-t* (preprocess get-def x) (preprocess get-def y))]
    [`(+ ,x ,y) (op-t+ (preprocess get-def x) (preprocess get-def y))]
    [`(sum-t-t ,w ,e) (op-sum-t-t w (preprocess get-def e))]
    [`(sum-i-t ,y ,e) (op-sum-i-t y (preprocess get-def e))]
    [`(,f ,vs ...) (apply (get-def f) (map (curry preprocess get-def) vs))]))
