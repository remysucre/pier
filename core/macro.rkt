#lang rosette

(provide (all-defined-out))

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define rel (make-hash))
(define var (make-hash))
(define fun (make-hash))
;; TODO swap x-type and type-x
(define var-type (make-hash))
(define rel-type (make-hash))
(define fun-type (make-hash))
(define type-var (make-hash))

(define (types t)
  (match t [`(~> ,ts ... ,t) (cons ts t)]))

(define-syntax-rule (decl kind x ... type)
  (begin
    (define-symbolic x ... type)
    (hash-set! kind 'x x) ...
    (if (hash-ref var (car (list 'x ...)) #f)
        (begin (hash-set! var-type 'type (list x ...))
               (hash-set! type-var x 'type) ... )
        (hash-set! rel-type (types 'type) (list x ...)))))

(define-syntax-rule (def op (f x ...) e)
  (begin
    (define (f x ...) e)
    (hash-set! op 'f f)
    (hash-set! fun-type 'f (list (hash-ref type-var x) ...))))
