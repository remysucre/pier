#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define S-237
  (op-+ (op-sum 't
                (op-* (op-I (rel-E 'v 't))
                      (op-delta 's 'v 't)))
        (op-sum 'u
                (op-sum 't
                        (op-* (op-I (rel-E 'v 't))
                              (op-* (op-delta 's 'v 't)
                                    (op-delta 's 't 'u)))))))

(define S-238
  (op-+ (op-sum 't (op-* (op-I (rel-E 'v 't))
                         (op-* (op-sigma 's 'v 't)
                               (op-inv (rel-sigma 's 't)))))
        (op-sum 'u
                (op-sum 't
                        (op-/ (op-* (op-* (op-I (rel-E 'v 't))
                                          (op-* (op-I (op-eq? (rel-D 's 'u)
                                                              (op-+ (rel-D 's 'v)
                                                                    (rel-D 'v 'u))))
                                                (op-I (op-eq? (rel-D 'v 'u)
                                                              (op-+ (rel-D 'v 't)
                                                                    (rel-D 't 'u))))))
                                    (op-* (rel-sigma 's 'v)
                                          (op-* (rel-sigma 'v 't)
                                                (rel-sigma 't 'u))))
                              (rel-sigma 's 'u))))))

(define-symbolic x y z integer?)
(assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))
(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))

;; NOTE the following is unsat
;; (assert (forall (list x) (=> (> x 0) (= (* x (inv x)) 1))))

;; NOTE try the following when z3 returns unknown
;; (assert (<= (D s u) (+ (D s t) (D t u))))
;; (assert (<= (D s t) (+ (D s v) (D v t))))
;; (assert (<= (D s u) (+ (D s v) (D v u))))
;; (assert (<= (D v u) (+ (D v t) (D t u))))

;; (verify (assert (not (= (interpret S-237) (interpret S-238)))))
(verify (assert (= (interpret S-237) (interpret S-238))))

(define (??v) (choose* 's 'u 'v 't))
(define (??op) (choose* op-+ op-*))

(define (??term depth)
  (if (= 0 depth)
      (choose* (??v)
               (op-I (rel-E (??v) (??v)))
               (op-delta (??v) (??v) (??v))
               (op-sum 't
                (op-* (op-I (rel-E 'v 't))
                      (op-delta 's 'v 't))))
      ((??op) (??term (- depth 1)) (??term (- depth 1)))))

(define sketch
  (op-+ (??term 0)
        (op-sum (??v)
                (op-sum 't
                        (op-* (op-delta 's 'v 't)
                              (??term 1))))))

(define M
  (synthesize
   #:forall (list E sigma D s t u v sig inv)
   #:guarantee (assert (eq? (interpret sketch) (interpret S-238)))))

(evaluate sketch M)
