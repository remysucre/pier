#lang rosette

(require rosette/lib/angelic) ; provides `choose*`
(require rosette/lib/synthax) ; provides `choose`
(require rosette/lib/value-browser)

(require "ops.rkt")

(provide (all-defined-out))

#;(define (old-gen-grammar type->var type->rel fun->type ops g)
  (define (??var t) (apply choose* (hash-ref type->var t)))

  (define ws (hash-ref type->var 'int? (list))) ;; weights
  (define (??v) (apply choose* (apply append (hash-values type->var))))         ;; all vars
  (define (??o) (apply choose* ops))

  (define (??rel)
    (define (gen-rel tr)
      (match tr
        [(cons (cons ts t) rs)
         (let ([r (op-rel (apply choose* rs) (map ??var ts))])
           (match t ['bool? (op-I r)] ['int? r]))]))
    (map gen-rel (hash->list type->rel)))

  (define (??fun)
    (define (gen-fun ft)
      (match ft [(cons f t) (op f (map ??var t))]))
    (map gen-fun (hash->list fun->type)))

  (define (??factor depth)
    (if (= 0 depth)
        (apply choose* (append ws (??rel) (??fun)))
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

  ;; FIXME only 1 level of aggregate
  (define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (op-sum w (op-* e (??factor 0)))
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 1))))))]))
  #;(define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (??term 0)
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 0))))))]))

  (sketch g))

(define (gen-grammar type->var type->rel fun->type ops g)
  (define (??var* t) (apply choose* (hash-ref type->var t)))

  (define ws (hash-ref type->var 'int? (list))) ;; weights
  (define (??v) (apply choose* (apply append (hash-values type->var))))         ;; all vars
  (define (??o) (apply choose* ops))

  (define (??rel)
    (define (gen-rel tr)
      (match tr
        [(cons (cons ts t) rs)
         (let ([r (apply (apply choose* rs) (map ??var* ts))])
           (match t ['bool? (I r)] ['int? r]))]))
    (map gen-rel (hash->list type->rel)))

  (define (??fun)
    (define (gen-fun ft)
      (match ft [(cons f t) (apply f (map ??var* t))]))
    (map gen-fun (hash->list fun->type)))

  (define (??factor depth)
    (if (= 0 depth)
        (apply choose* (append ws (??rel) (??fun)))
        ((??o) (??factor (- depth 1)) (??factor (- depth 1)))))

  (define (??term depth)
    (if (= 0 depth)
        (??factor 0)
        (choose* (??factor depth)
                 (op-sum (??v) (??term (- depth 1))))))

  (define (??agg depth e)
    (if (= depth 0)
      e
      (sum (??v) (??agg (- depth 1) e))))

  ;; FIXME only 1 level of aggregate
  (define (sketch g)
    (match g
      [(op-sum w e)
       (+ (sum w (* e (??factor 0)))
       ;; (+ (sum w e)
          (??agg 1
                 (sum w
                      (??agg 0
                             (* e (??factor 1))))))]))
  #;(define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (??term 0)
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 0))))))]))

  (sketch g))