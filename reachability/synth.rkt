#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define (??v) (choose* 'x 'y 'z 1))
(define (??op) (choose* op-+ op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??v)
               (rel-R (??v) (??v))
               (rel-E (??v) (??v)))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-exists (??v) (??term (- depth 1))))))

(define S-29
  (op-+ (rel-E 1 'y)
        (op-exists 'z
                   (op-* (rel-R 1 'z)
                         (rel-E 'z 'y)))))

;; ??term +
;; MIN_{?v ...}
;;   MIN_w1
;;     MIN_{?u ...} R(x, y, w1) * w1
;;                 * ??term

(define sketch
  (op-+ (??term 0)
        (op-exists (??v)
                   (op-* (rel-R 1 (??v))
                         (??term 0)))))

(define M
  (synthesize
   #:forall (list E R x y z sig)
   #:guarantee (assert (eq? (interpret sketch) (interpret S-29)))))

(evaluate sketch M)
