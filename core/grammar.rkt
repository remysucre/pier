#lang rosette

(require rosette/lib/angelic) ; provides `choose*`

(require "ops.rkt")

(provide (all-defined-out))

(define (gen-grammar type->var type->rel fun->type ops g)
  (define (??var t) (apply choose* (hash-ref type->var t)))
  (define (??vars ts)
    (let ([vss (apply cartesian-product (map (curry hash-ref type->var) ts))])
      (apply choose* (filter (negate check-duplicates) vss))))

  (define ws (hash-ref type->var 'int? (list))) ;; weights
  (define (??v) (apply choose* (apply append (hash-values type->var))))         ;; all vars
  (define (??o) (apply choose* ops))

  (define (??rel)
    (define (gen-rel tr)
      (match tr
        [(cons (cons ts t) rs)
         (let ([r (op-rel (apply choose* rs) (??vars ts) #;(map ??var ts))])
           (match t ['bool? (op-I r)] ['int? r]))]))
    (map gen-rel (hash->list type->rel)))

  (define (??fun)
    (define (gen-fun ft)
      (match ft [(cons f ts) (op f (??vars ts)#;(map ??var t))]))
    (map gen-fun (hash->list fun->type)))

  (define (??factor depth)
    (if (= 0 depth)
        (apply choose* (append ws (??rel) (??fun) (list 0 1)))
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
  ;; bc
  #;(define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (op-sum w (op-* e (??factor 0)))
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 1))))))]))

  ;; rt
  #;(define (sketch g)
    (match (g (??v) (??v) (??v))
      [(op-sum j (op-sum w e))
       (op-+ (??term 0)
             (op-sum j
                     (op-sum w
                             (op-* e (??factor 0)))))]))

  ;; sp
  (define env (make-hash))
  (define (make-sketch g)
    (match g
      ;; specialize (??v) with type
      [(op-sum v e)
       (op-+ (op-+ (op-sum (??v)#;(hash-ref! env v (λ () (??v)))
                     (op-* (make-sketch e) (??factor 0)))
                   (??term 0))
             (??agg 1
                    (op-sum (??v)#;(hash-ref! env v (λ () (??v)))
                            (??agg 0 (op-* (??factor 0)
                                           (make-sketch e))))))]
      [(op-* x y) (op-* (make-sketch x) (make-sketch y))]
      [(op-rel R xs) (op-rel R (map make-sketch xs))]
      [(op-I r) (op-I (make-sketch r))]
      [(constant _ _) (??v)#;(hash-ref! env g (λ () (??v)))]))
  (define (sketch g)
    (make-sketch g)
    #;(match (g (??v) (??v) (apply choose* ws))
      [(op-sum w e)
       (op-+ (op-sum w (op-* e (??factor 0)))
             (op-+ (??term 0)
             (??agg 1
                    (op-sum w
                            (??agg 0
                                   (op-* e (??factor 0)))))))]))
  ;; tc
  #;(define (sketch g)
    (match (g (??v))
      [e (op-+ (??term 0)
               (??agg 1 (op-* e (??factor 0))))]))

  ;; sw
  #;(define (sketch g)
    (op-+ (op-- (??term 0) (??term 0))
          (g (??v) (??v) (??v) (??v))))

  (sketch g))
