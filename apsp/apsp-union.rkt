#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))
(define (??op) (choose* op-t+ op-t*))

(define (??term depth)
  (if (= 0 depth)
      (choose* #;(op-I (rel-R (??v) (??v) (??w)))
               (op-I (rel-E (??v) (??v) (??w)))
               (??w)
               (op-weight (??w) (??v) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-I (??term (- depth 1)))
               (op-sum (??w)
                       (??term (- depth 1)))
               (op-sum-int (??v)
                       (??term (- depth 1))))))

(define S-29
  (op-t+ (op-weight 'w 'x 'z)
         (op-sum-int 'y
                     (op-sum 'w1
                             (op-sum 'w2
                                     (op-t* (op-I (rel-R 'x 'y 'w1))
                                            (op-t* (op-I (rel-E 'y 'x 'w2))
                                                   (op-t* 'w1 'w2))))))))

(define (??agg depth e)
  (if (= depth 0)
      e
      (choose* (op-sum-int (??v) (??agg (- depth 1) e))
               (op-sum (??w) (??agg (- depth 1) e)))))

(define sketch
  (op-t+ (??term 0)
         (??agg 1
                (op-sum 'w1
                        (??agg 1
                        (op-t* (op-t* (op-I (rel-R 'x 'y 'w1)) 'w1)
                               (??term 1)))))))

(define M
  (synthesize
   #:forall (list R-i-n R-n-n E-i-n E-n-n x y z w-i w1-i w2-i w-n w1-n w2-n sig-ii-n sig-ii-b sig-in-n sig-in-b sig-ni-n sig-ni-b sig-nn-n sig-nn-b sig-int-i-b sig-int-i-n sig-int-n-b sig-int-n-n)
   #:guarantee (assert (eq? (interpret sketch) (interpret S-29)))))

(evaluate sketch M)
