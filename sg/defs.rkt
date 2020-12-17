#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt")

(provide (all-defined-out))

;; symbolics

(define-symbolic E (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? boolean?))
(define-symbolic T (~> integer? boolean?))
(define-symbolic x y z p q integer?)

(define (S y)
  (op-+ (E 1 y)
        (op-exists 'z
                   (op-* (S 'z) (E 'z y)))))

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'p p)
        (cons 'q q)
        (cons 'z z)))
