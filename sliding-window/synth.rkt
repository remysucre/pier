#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define (??v) (choose* 'j 't 'k))
(define (??w) (choose* 'w))
(define (??op) (choose* op-+ op-- op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I (rel-R (??v) (??v) (??w)))
               (op-vec-get (??v) (??w) (??v))
               ;; FIXME hack here
               (op-vec-get (??v) (??w) (op-- (??v) (??v))))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum (??w) (??term (- depth 1)))
               (op-sum-int (??v) (??term (- depth 1))))))

(define (prefix t)
  (op-+ (op-sum-int 'j
                    (op-sum 'w
                            (op-* (op-* (op-I (rel-v 'j 'w))
                                        (op-* (op-I (op-leq 1 'j))
                                              (op-I (op-leq 'j t))))
                                  (op-* (op-I (op-eq? t 'j)) 'w))))
        (op-sum-int 'j
                    (op-sum 'w
                            (op-* (op-* 'w (op-I (rel-R (op-- t 1) 'j 'w)))
                                  (op-* (op-I (op-leq 1 'j))
                                        (op-* (op-I (op-leq 'j (op-- t 1)))
                                              (op-I (op-leq 1 (op-- t 1))))))))))

;; (define prefix-l
;;   (op-+ (op-sum-int 'j
;;                     (op-sum 'w
;;                             (op-* (op-* (op-I (rel-v 'j 'w))
;;                                         (op-* (op-I (op-leq 1 'j))
;;                                               (op-I (op-leq 'j (op-- 't 'k)))))
;;                                   (op-* (op-I (op-eq? (op-- 't 'k) 'j)) 'w))))
;;         (op-sum-int 'j
;;                     (op-sum 'w
;;                             (op-* (op-* 'w (op-I (rel-R (op-- (op-- 't 'k) 1) 'j 'w)))
;;                                   (op-* (op-I (op-leq 1 'j))
;;                                         (op-* (op-I (op-leq 'j (op-- (op-- 't 'k) 1)))
;;                                               (op-I (op-leq 1 (op-- (op-- 't 'k) 1))))))))))

(define sol
(verify (assert (not (eq? (interpret (op-+ (rel-window 't)
                                      (op-- (op-vec-get 'j 'w 't)
                                            (op-vec-get 'j 'w (op-- 't 'k)))))
                     (interpret (op-- (prefix 't) (prefix (op-- 't 'k)))))))))

;; ??term +
;; MIN_{?v ...}
;;   MIN_w1
;;     MIN_{?u ...} R(x, y, w1) * w1
;;                 * ??term
(define sketch
  ((??op) (??term 1)
          (rel-window 't)))

(define M
  (synthesize
   #:forall (list v R t j k w sig sig-int)
   #:guarantee (assert (eq? (interpret sketch) (interpret (op-- (prefix 't) (prefix (op-- 't 'k))))))))

(evaluate sketch M)
