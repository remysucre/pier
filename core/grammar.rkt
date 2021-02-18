#lang rosette

(require rosette/lib/angelic) ; provides `choose*`

(require "ops.rkt")

(provide (all-defined-out))

(define (gen-factor depth vs rels op)
  (define (??factor depth)
    (if (= 0 depth)
      ;; terminals include variables of the semiring type,
      ;; base relations and macros
      (apply choose* (cons vs rels))
      ;; non-terminals are semiring operations
      ((op) (??factor (- depth 1)) (??factor (- depth 1)))))
  (??factor depth))

;; same as factor, but also include aggregates
(define (gen-term depth vws agg term op)
  (if (= 0 depth)
      (term 0)
      (choose* ((op) (term (- depth 1)) (term (- depth 1)))
               (agg (apply choose* vws) (term (- depth 1))))))

(define (gen-agg depth e vws agg)
  (define (??agg depth e)
    (if (= depth 0)
      e
      (agg (apply choose* vws) (??agg (- depth 1) e))))
  (??agg depth e))


(define (gen-grammar var-type rel-type fun-type ops var rel fun g)
  (define (??v t) (apply choose* (hash-ref var-type t)))
  (define (??op) (apply choose* ops))
  (define (??factor depth)
    (gen-factor depth (??v 'int?)
                (append (gen-rel)
                        (gen-fun))
                ??op))
  (define (??term depth)
    (gen-term depth
              (map ??v (hash-keys var-type))
              op-sum ??factor ??op))
  (define (??agg depth e)
    (gen-agg depth e (map ??v (hash-keys var-type)) op-sum))

 (define (gen-rel)
    (define (gr r)
      (match r
        [(cons ts t)
         (let ([rl (op-rel (apply choose* (hash-ref rel-type r)) (map ??v ts))])
           (match t
             ['bool? (list (op-I rl))]
             ['int? (list rl)]))]))
    (map gr (hash-keys rel-type)))

  (define (gen-fun)
    (define (gf f)
      (op (hash-ref fun f) (map ??v (hash-ref fun-type f))))
    (map gf (hash-keys fun)))

  #;(sum w1 (* (* (I (R x y w1)) w1)))

  (define (sketch g)
    (match g
      [(op-sum w e)
       (op-+ (??term 0)
        (??agg 1
               (op-sum w
                       (??agg 0
                              (op-* e (??factor 0))))))]))

  (sketch g))
