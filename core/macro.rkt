#lang rosette

(provide (all-defined-out))

(require "interpret.rkt" "grammar.rkt" "ops.rkt")

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define rel (make-hash)) ;; symbol->rel
(define var (make-hash)) ;; symbol->var
(define fun (make-hash)) ;; symbol->fun
(define var->symbol (make-hash)) ;; symbol->var
(define fun->symbol (make-hash)) ;; symbol->var
(define rel->symbol (make-hash)) ;; symbol->var

(define type->var (make-hash))
(define type->rel (make-hash))
(define fun->type (make-hash))
(define var->type (make-hash))

(define (types t)
  (match t [`(~> ,ts ... ,t) (cons ts t)]))

(define-syntax-rule (decl kind x ... type)
  (begin
    (define-symbolic x ... type)
    (hash-set! kind 'x x) ...
    (match 'kind
      ['var (begin (hash-set! var->symbol x 'x) ...
                   (hash-set! type->var 'type (list x ...))
                   (hash-set! var->type x 'type) ... )]
      ['rel (begin
              (hash-set! rel->symbol x 'x) ...
              (hash-set! type->rel (types 'type) (list x ...)))])))

(define-syntax-rule (def (f x ...) e)
  (begin (define (f x ...) e)
         (hash-set! fun 'f f)
         (hash-set! fun->symbol f 'f)
         (hash-set! fun->type f (list (hash-ref var->type x) ...))))

(define (optimize p g)
  (define sketch (gen-grammar type->var
                              type->rel
                              fun->type
                              #;(list op-+ op-*)
                              (list op-+ op-* op-/)
                              g))

  (define M
    (synthesize
     #:forall (append (hash-values rel)
                      (hash-values var)
                      (hash-values fun)
                      (list sum inv))
     #:guarantee (assert (eq? (interpret sketch) p))))

  (evaluate sketch M))
