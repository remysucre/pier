#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

(require "../ops.rkt"
         "interpret.rkt")

;; INPUT
;;
(define prog (op-* (op-I-BN (rel-G 't 'x))
                   (op-I-BN (op-leq 5 (op-sum-i-i
                                       'p
                                       (op-* (op-I-BN (rel-P 'p 'x 't))
                                             (op-I-BN (rel-P 'p 'y (op-+ 1 't)))))))))

(define opt (op-* (op-I-BN (rel-G 't 'x))
                   (op-I-BN (op-leq 5 (op-sum-i-i
                                       'p
                                       (op-*
                                        (op-I-BN (rel-G 't 'x))
                                        (op-* (op-I-BN (rel-P 'p 'x 't))
                                             (op-I-BN (rel-P 'p 'y (op-+ 1 't))))))))))

(define p1 (op-* (op-I-BN (rel-G 't 'x))
                 (op-sum-i-i
                  'p
                  (op-* (op-I-BN (rel-P 'p 'x 't))
                        (op-I-BN (rel-G 't 'x))))))

(define p2 (op-* (op-I-BN (rel-G 't 'x))
                 (op-sum-i-i
                  'p
                  (op-I-BN (rel-P 'p 'x 't)))))

(define-symbolic v integer?)
(assert (forall (list v) (= (sum-i-i v 0) 0)))

(verify (assert (= (interpret prog) (interpret opt))))

;; GRAMMAR

(define (??v) (choose* 'x 'y 't 'p))
(define (??op) (choose* op-+ op-- op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??)
               (op-I-BN (rel-G (??v) (??v)))
               (op-I-BN (rel-P (??v) (??v) (??v))))
      (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
               (op-sum-i-i (??v) (??term (- depth 1)))
               (op-sum-i-i (??v) (??term (- depth 1))))))

(define sketch
  ((??op) (??term 1)
          (??term 1)))

#;(define M
  (synthesize
   #:forall (list v R t j k w sum-i-i)
   #:guarantee (assert (eq? (interpret sketch) (interpret #;prog (op-- (prefix 't) (prefix (op-- 't 'k))))))))

;;(evaluate sketch M)
