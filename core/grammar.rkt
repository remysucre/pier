#lang rosette

(require rosette/lib/synthax) ; provides `choose*`
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
      (match ft [(cons f ts)
                 (let ([vs (map ??var ts)])
                   (op f vs))]))
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

  (define env (make-hash))

  #;(define (sketch g)
    (define (sk g)
      (match g
      [(op-sum v e) (begin (hash-clear! env) (sk e))]
      [(op-* x y) (op-* (sk x) (sk y))]
      [(op-+ x y) (op-+ (sk x) (sk y))]
      [(op-- x y) (op-- (sk x) (sk y))]
      [(op-/ x y) (op-/ (sk x) (sk y))]
      [(op-inv x) (op-inv (sk x))]
      [(op-eq? x y) (op-eq? (sk x) (sk y))]
      [(op-leq x y) (op-leq (sk x) (sk y))]
      [(op-rel R xs) (op-rel R (map sk xs))]
      [(op f xs) (op f (map sk xs))]
      [(op-I r) (op-I (sk r))]
      [(? constant? g) (hash-ref! env g (λ () (??v)))]
      [_ g]))
    #;(op-+ (op-sum (??var 'id?) (op-* #;(sk g) (apply choose* (??fun))
                                     (apply choose* (??rel))))
          (op-sum (??var 'id?)
                  (op-sum (??var 'id?)
                          (op-* (sk g)
                                (op-* (apply choose* (??rel))
                                      (apply choose* (??fun)))))))
    (op-+ (??term 0)
          (op-sum (??v)
                  (op-sum (??v)
                          (op-* (sk g)
                                (??factor 0))))))

  ;; sp
  #;(define (sketch g)
    (match g
      ;; specialize (??v) with type
      [(op-sum v e)
       (op-+ (op-+ (op-sum (??v)#;(hash-ref! env v (λ () (??v)))
                     (op-* (sketch e) (??factor 0)))
                   (??term 0))
             (??agg 1
                    (op-sum (??v)#;(hash-ref! env v (λ () (??v)))
                            (??agg 0 (op-* (??factor 0)
                                           (sketch e))))))]
      [(op-* x y) (op-* (sketch x) (sketch y))]
      [(op-rel R xs) (op-rel R (map sketch xs))]
      [(op-I r) (op-I (sketch r))]
      [(constant _ _) (??v)#;(hash-ref! env g (λ () (??v)))]))

  ;; tc
  #;(define (sketch g)
    (match g
      [(op-I r)
       (op-+ (??term 0)
             (??agg 1
                    (op-* (??factor 0)
                          (op-I (sketch r)))))]
      [(op-rel R xs) (op-rel R (map sketch xs))]
      [(constant _ _) (??v)#;(hash-ref! env g (λ () (??v)))]
      [_ g]))

  ;; rt
  #;(define (sketch g)
    (match g
      [(op-sum j (op-sum w e))
       (op-+ (??term 0)
             (op-sum (??v)
                     (op-sum (??v)
                             (op-* (sketch e)
                                   (??factor 0)))))]
      [(op-* x y) (op-* (sketch x) (sketch y))]
      [(op-- x y) (op-- (sketch x) (sketch y))]
      [(op-leq x y) (op-leq (sketch x) (sketch y))]
      [(op-rel R xs) (op-rel R (map sketch xs))]
      [(op-I r) (op-I (sketch r))]
      [(constant _ _) (??v)#;(hash-ref! env g (λ () (??v)))]
      [_ g]))

  ;; sw
  (define (sketch g)
    (define (sk g)
      (match g
        [(op-sum v e) (op-sum (hash-ref! env g (λ () (??v))) (sk e))]
        [(op-* x y) (op-* (sk x) (sk y))]
        [(op-- x y) (op-- (sk x) (sk y))]
        [(op-leq x y) (op-leq (sk x) (sk y))]
        [(op-rel R xs) (op-rel R (map sk xs))]
        [(op-I r) (op-I (sk r))]
        [(constant _ _) (hash-ref! env g (λ () (??v)))]
        [_ g]
        ))
    (op-+ (op-- (??term 0) (??term 0))
          (sk g)))

  (sketch g))
