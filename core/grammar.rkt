#lang rosette

(require rosette/lib/angelic) ; provides `choose*`

(require "ops.rkt")

(provide (all-defined-out))

(define (gen-grammar type->var type->rel fun->type ops g)
  (define (??var t) (apply choose* (hash-ref type->var t)))

  (define (??w) (apply choose* (hash-ref type->var 'int?))) ;; weights
  (define (??v) (apply choose* (apply append (hash-values type->var))))         ;; all vars
  (define (??o) (apply choose* ops))

  (define (??factor depth)
    (if (= 0 depth)
        (apply choose* (cons (??w) (append (??rel) (??fun))))
        ((??o) (??factor (- depth 1)) (??factor (- depth 1)))))

  (define (??term depth)
    (if (= 0 depth)
        (??factor 0)
        (choose* (??factor depth)
                 (op-sum (??v) (??term (- depth 1))))))

  (define (??agg depth e)
    (if (= depth 0)
      e
      (op-sum (??v) (??agg (- depth 1) e))))

  (define (??rel)
    (define (gen-rel tr)
      (match tr
        [(cons (cons ts t) rs)
         (let ([r (op-rel (apply choose* rs) (map ??var ts))])
           (match t
             ['bool? (list (op-I r))]
             ['int? (list r)]))]))
    (map gen-rel (hash->list type->rel)))

  (define (??fun)
    (define (gf ft)
      (match ft
        [(cons f t) (op f (map ??var t))]))
    (map gf (hash->list fun->type)))

  ;; FIXME only 1 level of aggregate
  (define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (??term 0)
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 0))))))]))

  (sketch g))
