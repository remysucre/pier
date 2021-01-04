#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt")

(provide (all-defined-out))

(define (interpret p)
  (destruct p
    [(op-t* x y)
     (t* (interpret x )
         (interpret y))]
    [(op-t+ x y)
     (t+ (interpret x)
         (interpret y))]
    [(op-sum v e)
     (sig (interpret v)
          (interpret e))]
    [(op-sum-int v e)
     (sig-int (interpret v)
              (interpret e))]
    [(op-weight w x y)
     (weight (interpret w)
             (interpret x)
             (interpret y))]
    [(op-I e)
     (I (interpret e))]
    [(rel-E x y w)
     (E (interpret x) (interpret y) (interpret w))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))
