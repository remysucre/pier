#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

(require "../ops.rkt"
         "interpret.rkt")

;; INPUT
;;
(define prog
(-
 (+
  (sum 'j
   (sum 'w (* (* 'w (* (I (leq 1 'j)) (I (leq 'j 't))))
	      (* (I (eq? 'j 't)) (I (v 'j 'w))))))
  (sum 'j
   (sum 'w
    (* (* 'w (* (I (leq 1 'j)) (I (leq 'j 't))))
       (* (I (R (- 't 1) 'j 'w))
	  (* (I (lt 'j 't)) (I (gt 't 1))))))))
 (+ (sum 'j
	 (sum 'w
	      (* (* 'w (* (I (leq 1 'j)) (I (leq 'j (- 't 'k)))))
		 (* (I (v 'j 'w)) (I (eq? 'j (- 't 'k)))))))
  (sum 'j
   (sum 'w
    (* (* 'w (* (I (leq 1 'j)) (I (leq 'j (- 't 'k)))))
       (* (I (R (- (- 't 'k) 1) 'j 'w))
	  (* (I (lt 'j (- 't 'k))) (I (gt (- 't 'k) 1))))))))))

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

(define (??v) (choose* 'j 't 'k (op-- 't 'k) (op-- (op-- 't 'k) 1)))
(define (??w) (choose* 'w))
(define (??op) (choose* op-+ op-- op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??w)
               (op-I-BN (rel-R (??v) (??v) (??w)))
               (op-I-BN (rel-v (??v) (??w)))
               (op-vec-get (??v) (??w) (??v)))
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
