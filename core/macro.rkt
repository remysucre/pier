#lang rosette

(provide (all-defined-out))

(require "interpret.rkt" "grammar.rkt" "ops.rkt" "process.rkt" "serialize.rkt")

(define id? integer?)
(define int? integer?)
(define bool? boolean?)

(define rel (make-hash)) ;; symbol->rel
(define var (make-hash)) ;; symbol->var
(define fun (make-hash)) ;; symbol->fun
(define var->symbol (make-hash))
(define fun->symbol (make-hash))
(define rel->symbol (make-hash))

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
    ;; setting rel, var, fun
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
    (define (fun r)
    (λ (x ...)
      (begin
        (define args (make-hash (list (cons 'x x) ...)))
        (define (punctuate p)
          (match p
            [(? symbol?) (hash-ref args p p)]
            [(cons o xs) (if (eq? o 'r)
                             (apply r (map punctuate xs))
                             (cons o (map punctuate xs)))]
            [_ p]))
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
                               (cons o (map punctuate xs)))]
              [_ p])
            )
          (punctuate 'e))))
    (hash-set! meta 'g fun)
    (hash-set! meta 'g-args (list 'x ...))))

(define (optimize)
  (define prog
    (let* ([g (hash-ref meta 'g)]
           [f (hash-ref meta 'f)]
           [r (hash-ref meta 'r)]
           [xs (hash-ref meta 'g-args)]
           [norm (λ (p) (normalize p var rel fun))]
           [prep (λ (p) (preprocess p var rel fun))]
           [p (apply (g (f r)) xs)])
      (interpret (prep (deserialize (norm p))))))

  ;; rt
  ;; (define (g-R t j w) ; all variables in g
  ;;   (define vs (hash 't t 'j j 'w w))
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r t j w) `(I (R ,t ,j ,w)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (define (prep p) (preprocess p vs rel fun))
  ;;   (prep (norm ((g r) 't))))

  ;; (define (g-n)
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r t j w) `(I (R ,t ,j ,w)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (norm ((g r) '?t)))

  ;; sw
  ;; (define (g-R t j w k) ; all variables in g
  ;;   (define vs (hash 't t 'j j 'w w 'k k))
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r t j w) `(I (R ,t ,j ,w)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (define (prep p) (preprocess p vs rel fun))
  ;;   (prep (norm ((g r) 't 'k))))

  ;; (define (g-n)
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r t j w) `(I (R ,t ,j ,w)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (norm ((g r) '?t 'k)))


  ;; tc
  ;; (define (g-R y) ; all variables in g
  ;;   (define vs (hash 'y y))
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r x y) `(I (R ,x ,y)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (define (prep p) (preprocess p vs rel fun))
  ;;   (prep (norm ((g r) 'y))))

  ;; (define (g-n)
  ;;   (define g (hash-ref meta 'g))
  ;;   (define (r x y) `(I (R ,x ,y)))
  ;;   (define (norm p) (normalize p var rel fun))
  ;;   (norm ((g r) 'y)))

  ;; sp
  (define (g-R) ; all variables in g
    (define g (hash-ref meta 'g))
    (define r (hash-ref meta 'r))
    (define (norm p) (normalize p var rel fun))
    (define (prep p) (preprocess p var rel fun))
    (define xs (hash-ref meta 'g-args))
    (prep (norm (apply (g r) xs))))

  ;; "return" stratum
  #;(ret (g R)
         ;; S[x,z] = min w . R(x,z,w) + w.
         (λ (x z) (sum w (* (R x z w) w))))

  (define (g-n)
    (define g (hash-ref meta 'g))
    (define r (hash-ref meta 'r))
    (define (norm p) (normalize p var rel fun))
    (norm ((g r) 'x 'z)))

  (define (rewrite)
    (define (? x) `(var ,(string->symbol (~s '? x #:separator ""))))
    ;; TODO need to remove var for sw
    #;(define (? x) (string->symbol (~s '? x #:separator "")))
    (define xs (hash-ref meta 'g-args))
    (define lhs (make-pattern (serialize (g-n) rel var fun)))
    (~s lhs `(S ,@(map ? xs)) #:separator " => "))

  (define sketch
    (gen-grammar type->var type->rel fun->type
                 #;(list op-+ op-*)
                 (list op-+ op-* op--)
                 #;(list op-+ op-* op-/)
                 (g-R)))

  (define M
    (synthesize
     #:forall (append (hash-values rel)
                      (hash-values var)
                      (hash-values fun)
                      (list sum inv))
     #:guarantee (assert (eq? (interpret sketch) prog))))

  (define hg (serialize (postprocess (evaluate sketch M)
                                     var->symbol
                                     rel->symbol
                                     fun->symbol)
                        rel var fun))

  (display (extract (rewrite) hg)))
