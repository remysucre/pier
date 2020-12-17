#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define (??v) (choose* 'x 'y 'z (??)))
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

;;

(define S-1
  (op-+ (op-exists 'x (op-* (op-= 'x 1) (op-= 'x 'y)))
        (op-exists 'x
        (op-exists 'p
                   (op-exists 'q
                              (op-* (op-* (rel-R 'p 'q)
                                          (op-= 'x 1))
                                    (op-* (rel-E 'x 'p)
                                          (rel-E 'y 'q))))))))

(define S-M
  (op-+ (op-exists 'x (op-* (op-= 'x 1)
              (op-* (op-= 'x 'y) (rel-T 'x))))
        (op-exists 'x (op-exists 'p
                   (op-exists 'q
                              (op-* (op-* (rel-T 'x)
                                          (op-= 'x 1))
                                    (op-* (rel-R 'p 'q)
                                          (op-* (rel-E 'x 'p)
                                                (rel-E 'y 'q)))))))))

(define-symbolic v u integer?)
(assert (T 1))
;; (assert (forall (list v) (=> (&& (T u) (E u v)) (T v))))

(verify (assert (<=> (interpret S-1) (interpret  S-M))))

;; ??term +
;; MIN_{?v ...}
;;   MIN_w1
;;     MIN_{?u ...} R(x, y, w1) * w1
;;                 * ??term

;; (define sketch
;;   (op-+ (??term 0)
;;         (op-exists (??v)
;;                    (op-* (rel-R 1 (??v))
;;                          (??term 0)))))

;; (define M
;;   (synthesize
;;    #:forall (list E R x y z sig)
;;    #:guarantee (assert (eq? (interpret sketch) (interpret S-29)))))

;; (evaluate sketch M)
