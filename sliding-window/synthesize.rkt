#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

(require "../ops.rkt"
         "interpret.rkt")

;; INPUT
;;
(define prog
(op--
 (op-+
  (op-sum-i-i
   'j
   (op-sum-i-i 'w (op-* (op-* 'w (op-* (op-I-BN (op-leq 1 'j)) (op-I-BN (op-leq 'j 't)))) (op-* (op-I-BN (op-eq? 'j 't)) (op-I-BN (rel-v 'j 'w))))))
  (op-sum-i-i
   'j
   (op-sum-i-i
    'w
    (op-*
     (op-* 'w (op-* (op-I-BN (op-leq 1 'j)) (op-I-BN (op-leq 'j 't))))
     (op-* (op-I-BN (rel-R (op-- 't 1) 'j 'w)) (op-* (op-I-BN (op-lt 'j 't)) (op-I-BN (op-gt 't 1))))))))
 (op-+
  (op-sum-i-i
   'j
   (op-sum-i-i
    'w
    (op-*
     (op-* 'w (op-* (op-I-BN (op-leq 1 'j)) (op-I-BN (op-leq 'j (op-- 't 'k)))))
     (op-* (op-I-BN (rel-v 'j 'w)) (op-I-BN (op-eq? 'j (op-- 't 'k)))))))
  (op-sum-i-i
   'j
   (op-sum-i-i
    'w
    (op-*
     (op-* 'w (op-* (op-I-BN (op-leq 1 'j)) (op-I-BN (op-leq 'j (op-- 't 'k)))))
     (op-* (op-I-BN (rel-R (op-- (op-- 't 'k) 1) 'j 'w)) (op-* (op-I-BN (op-lt 'j (op-- 't 'k))) (op-I-BN (op-gt (op-- 't 'k) 1))))))))))

(define (prefix t)
  (op-+ (op-sum-i-i 'j
                    (op-sum-i-i 'w
                            (op-* (op-* (op-I-BN (rel-v 'j 'w))
                                        (op-* (op-I-BN (op-leq 1 'j))
                                              (op-I-BN (op-leq 'j t))))
                                  (op-* (op-I-BN (op-eq? t 'j)) 'w))))
        (op-sum-i-i 'j
                    (op-sum-i-i 'w
                            (op-* (op-* 'w (op-I-BN (rel-R (op-- t 1) 'j 'w)))
                                  (op-* (op-I-BN (op-leq 1 'j))
                                        (op-* (op-I-BN (op-lt 'j t))
                                              (op-I-BN (op-lt 1 t)))))))))

;; (define (prefix t)
;;   (op-+ (op-sum-i-i 'j
;;                     (op-sum-i-i 'w
;;                             (op-* (op-* (op-I-BN (rel-v 'j 'w))
;;                                         (op-* (op-I-BN (op-leq 1 'j))
;;                                               (op-I-BN (op-leq 'j t))))
;;                                   (op-* (op-I-BN (op-eq? t 'j)) 'w))))
;;         (op-sum-i-i 'j
;;                     (op-sum-i-i 'w
;;                             (op-* (op-* 'w (op-I-BN (rel-R (op-- t 1) 'j 'w)))
;;                                   (op-* (op-I-BN (op-leq 1 'j))
;;                                         (op-* (op-I-BN (op-leq 'j (op-- t 1)))
;;                                               (op-I-BN (op-leq 1 (op-- t 1))))))))))

;; GRAMMAR

(define (??v) (choose* 'j 't 'k))
(define (??w) (choose* 'w))
(define (??op) (choose* op-+ op-- op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I-BN (rel-R (??v) (??v) (??w)))
               (op-vec-get (??v) (??w) (??v))
               ;; FIXME hack here
               (op-vec-get (??v) (??w) ((??op) (??v) (??v))))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum-i-i (??w) (??term (- depth 1)))
               (op-sum-i-i (??v) (??term (- depth 1))))))

(define sketch
  ((??op) (??term 1)
          (rel-window 't)))

(define M
  (synthesize
   #:forall (list v R t j k w sum-i-i)
   #:guarantee (assert (eq? (interpret sketch) (interpret #;prog (op-- (prefix 't) (prefix (op-- 't 'k))))))))

(evaluate sketch M)
