#lang rosette

(require "ops.rkt")

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
      [`(sum ,y ,e) (op-sum (hash-ref var y) (prep e))]
      [`(,f ,vs ...) (apply (hash-ref rel f (lambda () (hash-ref fun f)))
                            (map prep vs))]))
  (prep e))
