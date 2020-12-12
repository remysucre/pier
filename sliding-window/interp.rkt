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
    [(op-- x y)
     (- (interpret x)
        (interpret y))]
    [(op-eq? x y)
     (eq? (interpret x)
          (interpret y))]
    [(op-leq x y)
     (<= (interpret x)
         (interpret y))]
    [(op-sum v e)
     (sig (interpret v)
          (interpret e))]
    [(op-sum-int v e)
     (sig-int (interpret v)
              (interpret e))]
    [(op-vec-get j w t)
     (interpret
     (vec-get (interpret j)
              (interpret w)
              (interpret t)))]
    [(op-I e)
     (I (interpret e))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [(rel-v i x)
     (v (interpret i) (interpret x))]
    [(rel-S t)
     (interpret (S (interpret t)))]
    [(rel-window t)
     (interpret (window (interpret t)))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))
