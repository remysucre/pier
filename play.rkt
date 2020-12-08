#lang rosette/safe

(require rosette/lib/angelic
         rosette/lib/destruct)

(define (w) (define-symbolic x real?) x)
(define (u) (define-symbolic x real?) x)

(verify (assert (eq? (w) (w))))
