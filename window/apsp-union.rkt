#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define (??v) (choose* 'j 't))
(define (??w) (choose* 'w))
(define (??op) (choose* op-+ op-- op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I (rel-R (??v) (??v) (??w)))
               (op-vec-get (??v) (??w) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum (??w) (??term (- depth 1)))
               (op-sum-int (??v) (??term (- depth 1))))))

(define S-29
  (op-+ (op-sum-int 'j
                    (op-sum 'w
                            (op-* (op-* (op-I (rel-v 'j 'w))
                                        (op-* (op-I (op-leq 1 'j))
                                              (op-I (op-leq 'j 't))))
                                  (op-* (op-I (op-eq? 't 'j)) 'w))))
        (op-sum-int 'j
                    (op-sum 'w
                            (op-*
                             (op-* 'w (op-I (rel-R (op-- 't 1) 'j 'w)))
                             (op-* (op-I (op-leq 1 'j))
                                   (op-* (op-I (op-leq 'j (op-- 't 1)))
                                         (op-I (op-leq 1 (op-- 't 1))))))))))

(verify (assert (eq? (interpret S-29)
                     (interpret (op-+ (rel-S (op-- 't 1))
                                      (op-vec-get 'j 'w 't))))))

;; ??term +
;; MIN_{?v ...}
;;   MIN_w1
;;     MIN_{?u ...} R(x, y, w1) * w1
                 * ??term
(define sketch
  ((??op) (??term 0)
          (rel-S (op-- 't 1))))

(define M
  (synthesize
   #:forall (list v R t j w sig sig-int)
   #:guarantee (assert (eq? (interpret sketch) (interpret S-29)))))

(evaluate sketch M)
