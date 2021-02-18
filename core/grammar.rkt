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

;; same as term, but also include aggregates
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


(define (gen-grammar var-type rel-type ops var rel fun)
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
    (list (op-I (op-rel (choose* (hash-ref rel 'E) (hash-ref rel 'R)) (list (??v 'id?) (??v 'id?) (??v 'int?))))))
  #;(define (gen-rel)
    (define (gr r)
      (match r
        [(cons ts t)
         (let ([rl (op-rel (apply choose* (hash-ref rel-type r)) (map ??v ts))])
           (rl))]))
    (map gr (hash-keys rel)))

  (define (gen-fun)
    (list (op (hash-ref fun 'weight) (list (??v 'int?) (??v 'id?) (??v 'id?)))))

  (define sketch
    (op-+ (??term 0)
        (??agg 1
               (op-sum (hash-ref var 'w1)
                       (??agg 0
                              (op-* (op-* (op-I (op-rel (hash-ref rel 'R) (list (hash-ref var 'x)
                                                                                (hash-ref var 'y)
                                                                                (hash-ref var 'w1))))
                                          (hash-ref var 'w1))
                                    (??factor 0)))))))
  sketch)
