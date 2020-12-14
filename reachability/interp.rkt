#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt")

(provide (all-defined-out))

(define (interpret p)
  (destruct p
    [(op-* x y)
     (&& (interpret x )
         (interpret y))]
    [(op-+ x y)
     (|| (interpret x)
         (interpret y))]
    [(op-exists v e)
     (sig (interpret v)
          (interpret e))]
    [(rel-R x y)
     (R (interpret x) (interpret y))]
    [(rel-E x y)
     (E (interpret x) (interpret y))]
    [(rel-S t)
     (interpret (S (interpret t)))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))
