#lang rosette

(provide (all-defined-out))

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define rel (box (list)))
(define var (box (list)))
(define fun (box (list)))
(define var-type (box (list)))
(define rel-type (box (list)))

(define-syntax-rule (decl kind x ... type)
  (begin
    (define-symbolic x ... type)
    (set-box! kind (append (list (cons 'x x) ... ) (unbox kind)))
    (if (assoc (car (list 'x ...)) (unbox var))
        (set-box! var-type (cons (cons 'type (list 'x ...)) (unbox var-type)))
        (set-box! rel-type (append (list (cons 'x 'type) ... ) (unbox rel-type))))))

(define-syntax-rule (def op (f xs ...) e)
  (begin
    (define (f xs ...) e)
    (set-box! op (cons (cons 'f f) (unbox op)))))
