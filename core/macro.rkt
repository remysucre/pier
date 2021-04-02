#lang rosette

(provide (all-defined-out))

(require "interpret.rkt" "grammar.rkt" "ops.rkt" "process.rkt" "serialize.rkt")

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

(define meta (make-hash))

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

(define-syntax-rule (rec (fun r) (λ (x ...) e))
  (begin
    (hash-set! meta 'base (curry list 'r))
    (define (fun r)
    (λ (x ...)
      (begin
        (define args (make-hash (list (cons 'x x) ...)))
        (define (punctuate p)
          (match p
            [(? symbol?) (hash-ref args p p)]
            [(cons o xs) (if (eq? o 'r)
                             (apply r (map punctuate xs))
                             (cons o (map punctuate xs)))]))
        (punctuate 'e))))
    (hash-set! meta 'f fun)))

(define-syntax-rule (ret (fun s) (λ (x ...) e))
  (begin
    (define (fun s)
      (λ (x ...)
        (begin
          (define args (make-hash (list (cons 'x x) ...)))
          (define (punctuate p)
            (match p
              [(? symbol?) (hash-ref args p p)]
              [(cons o xs) (if (eq? o 's)
                               (apply s (map punctuate xs))
                               (cons o (map punctuate xs)))])
            )
          (punctuate 'e))))
    (hash-set! meta 'g fun)
    (hash-set! meta 'g-args (list 'x ...))))

(define (optimize)
  (define prog
    (let* ([g (hash-ref meta 'g)]
           [f (hash-ref meta 'f)]
           [r (hash-ref meta 'base)]
           [xs (hash-ref meta 'g-args)]
           [norm (λ (p) (normalize p var rel fun))]
           [prep (λ (p) (preprocess p var rel fun))]
           [p (apply (g (f r)) xs)])
      (interpret (prep (deserialize (norm p))))))

  (define (g-R x z w) ; all variables in g
    (define vs (hash 'x x 'z z 'w w))
    (define g (hash-ref meta 'g))
    (define (r x y z) `(I (R ,x ,y ,z)))
    (define (norm p) (normalize p var rel fun))
    (define (prep p) (preprocess p vs rel fun))
    (prep (norm ((g r) 'x 'z))))

  (define (g-n)
    (define g (hash-ref meta 'g))
    (define (r x y z) `(I (R ,x ,y ,z)))
    (define (norm p) (normalize p var rel fun))
    (norm ((g r) 'x 'z)))

  (define rewrite
    (string-append (pretty-format (make-pattern (serialize (g-n) rel var fun)) 'infinity #:mode 'display)
                   " => "
                   (pretty-format `(S ,@(map (λ (x) `(var ,(string->symbol (string-append "?" (symbol->string x))))) (hash-ref meta 'g-args)))
                                  'infinity
                                  #:mode 'display)))

  (define sketch (gen-grammar type->var
                              type->rel
                              fun->type
                              #;(list op-+ op-*)
                              (list op-+ op-* op-/)
                              g-R))

  (define M
    (synthesize
     #:forall (append (hash-values rel)
                      (hash-values var)
                      (hash-values fun)
                      (list sum inv))
     #:guarantee (assert (eq? (interpret sketch) prog))))

  (define h-g (evaluate sketch M))
  (define hg (serialize (postprocess h-g var->symbol rel->symbol fun->symbol) rel var fun))
  (display (extract rewrite hg)))
