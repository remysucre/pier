#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

(define S-237
  (op-+ (op-sum 't (op-* (op-I (rel-E 'v 't))
                         (op-delta 's 'v 't)))
        (op-sum 'u
                (op-sum 't
                        (op-/ (op-* (op-* (op-I (rel-E 'v 't))
                                          (op-* (op-I (op-eq? (rel-D 's 'u)
                                                              (op-+ (rel-D 's 't)
                                                                    (rel-D 't 'u))))
                                                (op-I (op-eq? (rel-D 's 't)
                                                              (op-+ (rel-D 's 'v)
                                                                    (rel-D 'v 't))))))
                                    (op-* (rel-sigma 's 'v)
                                          (op-* (rel-sigma 'v 't)
                                                (rel-sigma 't 'u))))
                              (rel-sigma 's 'u))))))

(define S-238
  (op-+ (op-sum 't (op-* (op-I (rel-E 'v 't))
                         (op-delta 's 'v 't)))
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

(define l



  (op-+ (op-sum 't (op-* (op-I (rel-E 'v 't))
                         (op-delta 's 'v 't)))
        (op-sum 'u
                (op-sum 't
                        (op-* (op-* (op-* (op-I (rel-E 'v 't))
                                          (op-* (op-I (op-eq? (rel-D 's 'u)
                      (op-+ (rel-D 's 't)
                            (rel-D 't 'u))))
        (op-I (op-eq? (rel-D 's 't)
                      (op-+ (rel-D 's 'v)
                            (rel-D 'v 't))))))
                                    (op-* (rel-sigma 's 'v)
                                          (op-* (rel-sigma 'v 't)
                                                (rel-sigma 't 'u))))
                              (op-inv (rel-sigma 's 'u)))))))
(define r


 (op-+ (op-sum 't (op-* (op-I (rel-E 'v 't))
                         (op-delta 's 'v 't)))
        (op-sum 'u
                (op-sum 't

(op-* (op-* (op-* (op-I (rel-E 'v 't))
                                          (op-* (op-I (op-eq? (rel-D 's 'u)
                      (op-+ (rel-D 's 'v)
                            (rel-D 'v 'u))))
        (op-I (op-eq? (rel-D 'v 'u)
                      (op-+ (rel-D 'v 't)
                            (rel-D 't 'u))))))
                                    (op-* (rel-sigma 's 'v)
                                          (op-* (rel-sigma 'v 't)
                                                (rel-sigma 't 'u))))
                              (op-inv (rel-sigma 's 'u)))))))

(define lhs
  (op-* (op-I (op-eq? (rel-D 's 'u)
                      (op-+ (rel-D 's 't)
                            (rel-D 't 'u))))
        (op-I (op-eq? (rel-D 's 't)
                      (op-+ (rel-D 's 'v)
                            (rel-D 'v 't))))))

(define rhs
  (op-* (op-I (op-eq? (rel-D 's 'u)
                      (op-+ (rel-D 's 'v)
                            (rel-D 'v 'u))))
        (op-I (op-eq? (rel-D 'v 'u)
                      (op-+ (rel-D 'v 't)
                            (rel-D 't 'u))))))

;; (define-symbolic x y z integer?)

;; (require rosette/solver/smt/cvc4)
;; (current-solver (cvc4 #:path "/home/remywang/tools/cvc4-1.8-x86_64-linux-opt"))
;; (current-solver)

;; (assert (forall (list x y z) (<= (D x z) (+ (D x y) (D y z)))))

(assert (<= (D s u) (+ (D s t) (D t u))))
(assert (<= (D s t) (+ (D s v) (D v t))))
(assert (<= (D s u) (+ (D s v) (D v u))))
(assert (<= (D v u) (+ (D v t) (D t u))))

;; (verify (assert (eq? (interpret lhs) (interpret rhs))))
(verify (assert (eq? (interpret l) (interpret r))))

;; (define (??v) (choose* 'x 'y 'z (??)))
;; (define (??op) (choose* op-+ op-*))

;; (define (??term depth)
;;   (if (= 0 depth)
;;       (choose* (??v)
;;                (rel-R (??v) (??v))
;;                (rel-E (??v) (??v)))
;;       (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
;;                (op-exists (??v) (??term (- depth 1))))))

;; (define S-29
;;   (op-+ (rel-E 1 'y)
;;         (op-exists 'z
;;                    (op-* (rel-R 1 'z)
;;                          (rel-E 'z 'y)))))

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
