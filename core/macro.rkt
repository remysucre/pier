#lang rosette

(provide (all-defined-out))

(define rel (box (list)))
(define var (box (list)))
(define fun (box (list)))
(define vars (box (list)))

(define-syntax-rule (decl kind x ... type)
  (begin
    (define-symbolic x ... type)
    (set-box! kind (append (list (cons 'x x) ... ) (unbox kind)))
    (cond [(assoc (car (list 'x ...)) (unbox var))
           (set-box! vars (cons (list 'x ...) (unbox vars)))])))

(define-syntax-rule (def op (f xs ...) e)
  (begin
    (define (f xs ...) e)
    (set-box! op (cons (cons 'f f) (unbox op)))))
