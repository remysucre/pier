#lang rosette

(require rosette/lib/angelic) ; provides `choose*`

(provide (all-defined-out))

(define (gen-term depth vs rels op)
  (define (??term depth)
    (if (= 0 depth)
      ;; terminals include variables of the semiring type,
      ;; base relations and macros
      (apply choose* (cons vs rels))
      ;; non-terminals are semiring operations
      ((op) (??term (- depth 1)) (??term (- depth 1)))))
  (??term depth))

;; same as term, but also include aggregates
(define (gen-factor depth vws agg term op)
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
