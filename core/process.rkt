#lang rosette

(require "ops.rkt")

(provide (all-defined-out))

(define (preprocess get-def e)
  (match e
    [(? symbol? e) e]
    [`(I ,e) (op-I (preprocess get-def e))]
    #;[`(inv ,e) (op-inv (preprocess get-def e))]
    [`(* ,x ,y) (op-* (preprocess get-def x) (preprocess get-def y))]
    [`(+ ,x ,y) (op-+ (preprocess get-def x) (preprocess get-def y))]
    [`(sum ,y ,e) (op-sum y (preprocess get-def e))]
    [`(,f ,vs ...) (apply (get-def f) (map (curry preprocess get-def) vs))]))
