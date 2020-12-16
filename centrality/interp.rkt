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
     (* (interpret x )
        (interpret y))]
    [(op-+ x y)
     (+ (interpret x)
        (interpret y))]
    [(op-inv x)
     (inv (interpret x))]
    [(op-/ x y)
     (diiv (interpret x)
           (interpret y))]
    [(op-sum v e)
     (sig (interpret v)
          (interpret e))]
    [(op-eq? x y)
     (eq? (interpret x)
          (interpret y))]
    [(op-I x) (I (interpret x))]
    [(rel-D x y)
     (D (interpret x) (interpret y))]
    [(rel-sigma s t)
     (sigma (interpret s)
            (interpret t))]
    [(op-sigma s v t)
     (interpret (sigma-3 (interpret s)
                         (interpret v)
                         (interpret t)))]
    [(op-delta s v t)
     (interpret (delta (interpret s)
                       (interpret v)
                       (interpret t)))]
    [(rel-E x y)
     (E (interpret x) (interpret y))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))
