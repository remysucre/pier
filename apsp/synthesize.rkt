#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

(require "../ops.rkt"
         "interpret.rkt")

;; INPUT

(define prog
  (op-t+ (op-weight 'w 'x 'z)
         (op-sum-i-t 'y
                     (op-sum-t-t 'w1
                             (op-sum-t-t 'w2
                                     (op-t* (op-I-BT (rel-R 'x 'y 'w1))
                                            (op-t* (op-I-BT (rel-E 'y 'x 'w2))
                                                   (op-t* 'w1 'w2))))))))

;; GRAMMAR

(define (??v) (choose* 'x 'y 'z))
(define (??w) (choose* 'w 'w1 'w2))
(define (??op) (choose* op-t+ op-t*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I-BT (rel-E (??v) (??v) (??w)))
               (op-I-BT (rel-R (??v) (??v) (??w)))
               (op-weight (??w) (??v) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum-t-t (??w) (??term (- depth 1)))
               (op-sum-i-t (??v) (??term (- depth 1))))))

(define (??agg depth e)
  (if (= depth 0)
      e
      (choose* (op-sum-i-t (??v) (??agg (- depth 1) e))
               (op-sum-t-t (??w) (??agg (- depth 1) e)))))

(define sketch
  (op-t+ (??term 0)
         (??agg 1
                (op-sum-t-t 'w1
                        (??agg 1
                        (op-t* (op-t* (op-I-BT (rel-R 'x 'y 'w1)) 'w1)
                               (??term 1)))))))

(define M
  (synthesize
   #:forall (list R-i-n R-n-n E-i-n E-n-n
                  x y z
                  w-i w1-i w2-i w-n w1-n w2-n
                  sum-t-inf-inf-r sum-t-inf-inf-b sum-t-inf-r-r sum-t-inf-r-b sum-t-r-inf-r sum-t-r-inf-b sum-t-r-r-r sum-t-r-r-b
                  sum-t-i-inf-b sum-t-i-inf-i sum-t-i-i-b sum-t-i-i-i)
   #:guarantee (assert (eq? (interpret sketch) (interpret prog)))))

(evaluate sketch M)
