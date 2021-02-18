#lang rosette

(provide (all-defined-out))

(define rel (box (list)))
(define var (box (list)))
(define fun (box (list)))

(define-syntax-rule (decl kind name type)
  (begin
    (define-symbolic name type)
    (set-box! kind (cons (cons 'name name) (unbox kind)))))

(define-syntax-rule (def op (f xs ...) e)
  (begin
    (define (f xs ...) e)
    (set-box! op (cons (cons 'f f) (unbox op)))))
