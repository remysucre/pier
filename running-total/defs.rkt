#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt")

(provide (all-defined-out))

;; symbolics

(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))
(define-symbolic t j integer?)
(define-symbolic w integer?)

(define (vec-get j w t)
  (op-sum-int j
              (op-sum w
                      (op-* w
                            (op-* (op-* (op-I (op-eq? j t))
                                        (op-I (op-leq 1 t)))
                                  (op-I (rel-v j w)))))))

(define (S t)
  (op-sum-int 'j
              (op-sum 'w
                      (op-* (op-* 'w (op-I (rel-R t 'j 'w)))
                            (op-* (op-I (op-leq 1 'j))
                                  (op-I (op-leq 'j t)))))))

(define vars
  (list (cons 't t)
        (cons 'j j)
        (cons 'w w)))
