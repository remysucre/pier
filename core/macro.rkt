#lang rosette

(provide (all-defined-out))

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define rel (make-hash))
(define var (make-hash))
(define fun (make-hash))
(define var-type (make-hash))
(define rel-type (make-hash))

(define (types t)
  (match t [`(~> ,ts ... ,t) (cons ts t)]))

(define-syntax-rule (decl kind x ... type)
  (begin
    (define-symbolic x ... type)
    (hash-set! kind 'x x) ...
    (if (hash-ref var (car (list 'x ...)) #f)
        (hash-set! var-type 'type (list x ...))
        (begin (hash-set! rel-type (types 'type) x) ...))))

(define-syntax-rule (def op (f xs ...) e)
  (begin
    (define (f xs ...) e)
    (hash-set! op 'f f)))
