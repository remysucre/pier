#lang rosette

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provides `destruct`

(define-symbolic x y z a integer?)

(define sketch (choose* (lambda (x) x)
                        (lambda (y) x)
                        (lambda (z) x)))

(define M
  (synthesize
   #:forall (list a x y z)
   #:guarantee (assert (= (sketch a)
                          ((lambda (x) x) a)))))

(evaluate (sketch 9) M)
