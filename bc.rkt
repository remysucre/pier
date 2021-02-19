#lang rosette
(require "core/lib.rkt")

;; HACK shadowing D
(decl rel D (~> id? id? int?))
(decl rel E (~> id? id? bool?))
(decl rel sigma (~> id? id? int?))

(decl var s t u v id?)

#;(def (sig s v t)
  (* (* (sigma s v) (sigma v t))
     (I (eq? (D s t) (+ (D s v) (D v t))))))

(def (delta s v t)
  (* (* (* (sigma s v) (sigma v t))
        (I (eq? (D s t) (+ (D s v) (D v t)))))
     (inv (sigma s t))))

(assert (forall (list s t u) (<= (D s u) (+ (D s t) (D t u)))))
(assert (forall (list s t) (<=> (E s t) (= 1 (D s t)))))
(assert (forall (list s t) (<=> (E s t) (= 1 (sigma s t)))))

(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))

(define p
  (+ (sum t (* (I (E v t))
               (delta s v t)))
     (sum u
          (sum t
               (div (* (* (I (E v t))
                          (* (I (eq? (D s u)
                                     (+ (D s v) (D v u))))
                             (I (eq? (D v u)
                                     (+ 1 (D t u))))))
                       (* (sigma s v) (sigma t u)))
                    (sigma s u))))))

#;(define g (op-sum t (op-* (op delta (list s v t)) (I (E v t)))))
(define g (op-sum t (op delta (list s v t))))

(define opt (+ (sum t (* (I (E v t)) (delta s v t)))
               (sum u (sum t (* (I (E v t))
                                (* (delta s v t)
                                   (delta s t u)))))))

(optimize opt g)
