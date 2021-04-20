#lang rosette

(require rosette/lib/synthax) ; provides `choose*`
(require rosette/lib/angelic) ; provides `choose*`

(require "ops.rkt")

(provide (all-defined-out))

(define (gen-grammar type->var type->rel fun->type ops r p g)

  (define (rec? p)
    (match p
      [(op-rel _ xs) (or (eq? (r xs) p) (eq? (r xs) (op-I p)))]
      [(op _ xs) (or (map rec? xs))]
      [(op-I x) (rec? x)]
      [(op-&& x y) (or (rec? x) (rec? y))]
      [(op-|| x y) (or (rec? x) (rec? y))]
      [(op-+ x y) (or (rec? x) (rec? y))]
      [(op-- x y) (or (rec? x) (rec? y))]
      [(op-* x y) (or (rec? x) (rec? y))]
      [(op-/ x y) (or (rec? x) (rec? y))]
      [(op-inv x) (rec? x)]
      [(op-eq? x y) (or (rec? x) (rec? y))]
      [(op-leq x y) (or (rec? x) (rec? y))]
      [(op-sum _ e) (rec? e)]
      [(op-exists _ e) (rec? e)]
      [_ #f]))

  (define env (make-hash))

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

  (define (sketch p g)
    (if (rec? p)
        (match p
          [(op-+ x y)
           (match g
             [(op-+ a b) (op-+ (sketch x a) (sketch y b))]
             [_ (op-+ (sketch x g) (sketch y g))])]
          [(op-sum x e) (op-sum x (sketch e g))]
          [(op-* _ _) (op-* (??factor 0) (sk g))]
          [_ p])
        p))

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

  (sketch p g))
