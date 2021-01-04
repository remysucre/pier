#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

(require "../ops.rkt"
         "interpret.rkt")

;; INPUT
(define prog
  (op-+ (op-sum-i-i 't (op-* (op-I-BN (rel-E 'v 't))
                         (op-/ (op-sigma 's 'v 't)
                               (rel-sigma 's 't))))
        (op-sum-i-i 'u
                (op-sum-i-i 't
                        (op-/ (op-* (op-* (op-I-BN (rel-E 'v 't))
                                          (op-* (op-I-BN (op-eq? (rel-D 's 'u)
                                                              (op-+ (rel-D 's 'v)
                                                                    (rel-D 'v 'u))))
                                                (op-I-BN (op-eq? (rel-D 'v 'u)
                                                              (op-+ 1
                                                                    (rel-D 't 'u))))))
                                    (op-* (rel-sigma 's 'v) (rel-sigma 't 'u)))
                              (rel-sigma 's 'u))))))

(define S-237
  (op-+ (op-sum-i-i 't
                (op-* (op-I-BN (rel-E 'v 't))
                      (op-delta 's 'v 't)))
        (op-sum-i-i 'u
                (op-sum-i-i 't
                        (op-* (op-I-BN (rel-E 'v 't))
                              (op-* (op-delta 's 'v 't)
                                    (op-delta 's 't 'u)))))))

;; ASSERTS
(define-symbolic x y z integer?)
(assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))
(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))
(assert (forall (list x y) (<=> (E x y) (= 1 (D x y)))))
(assert (forall (list x y) (<=> (E x y) (= 1 (sigma x y)))))

;; (verify (assert (not (= (interpret S-237) (interpret prog)))))
;; (verify (assert (= (interpret S-237) (interpret prog))))

;; GRAMMAR

(define (??v) (choose* 's 'u 'v 't))
(define (??op) (choose* op-+ op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??v)
               (op-I-BN (rel-E (??v) (??v)))
               (op-delta (??v) (??v) (??v))
               (op-sum-i-i 't
                (op-* (op-I-BN (rel-E 'v 't))
                      (op-delta 's 'v 't))))
      ((??op) (??term (- depth 1)) (??term (- depth 1)))))

(define sketch
  (op-+ (??term 0)
        (op-sum-i-i (??v)
                (op-sum-i-i 't
                        (op-* (op-delta 's 'v 't)
                              (??term 1))))))

(define M
  (synthesize
   #:forall (list E sigma D s t u v sum-i-i inv)
   #:guarantee (assert (eq? (interpret sketch) (interpret prog)))))

(evaluate sketch M)
