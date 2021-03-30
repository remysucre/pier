#lang rosette

(provide (all-defined-out))

(define (serialize e rel var fun)
  (define (ser e)
    (match e
      [(? symbol? e) `(var ,e)]
      [`(sum ,v ,b) `(sum ,v ,(ser b))]
      [`(,f ,es ...) (cond [(hash-has-key? rel f) `(rel ,f ,@(map ser es))]
                           [else `(,f ,@(map ser es))])]
      [n n]))
  (ser e))
