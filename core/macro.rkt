#lang rosette

(provide (all-defined-out))

(require "interpret.rkt" "process.rkt" "grammar.rkt" "ops.rkt")

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define ler (make-hash))
(define rav (make-hash))
(define nuf (make-hash))

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
    (match 'kind
      ['rel (begin (hash-set! ler x 'x) ... )]
      ['var (begin (hash-set! rav x 'x) ... )])
    (if (hash-ref var (car (list 'x ...)) #f)
        (begin (hash-set! var-type 'type (list x ...))
               (hash-set! type-var x 'type) ... )
        (hash-set! rel-type (types 'type) (list x ...)))))

(define-syntax-rule (def op (f x ...) e)
  (begin
    (define (f x ...) e)
    (hash-set! op 'f f)
    (hash-set! nuf f 'f)
    (hash-set! fun-type 'f (list (hash-ref type-var x) ...))))

(define (optimize p g)
  (define sketch (gen-grammar var-type rel-type fun-type (list op-+ op-*) var rel fun (pr g var rel)))

  (define M
    (synthesize
     #:forall (append (hash-values rel) (hash-values var) (hash-values fun) (list sum))#;(list R E x y z w w1 w2 sum)
     #:guarantee (assert (eq? (interpret sketch) p))))

  (show (evaluate sketch M) ler nuf rav))
