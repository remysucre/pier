#lang rosette
(require "core/lib.rkt")

(decl rel V (~> id? int?))
(decl rel E T (~> id? id? bool?))
(decl var x y z id?)

(idb (t x y) `(I (rel T ,x ,y)))

(stratum (f t)
         (λ (x y)
           (sum z (* (t x z)
                     (I (rel E z y))))))

(assert (forall (list temp) (= (sum temp 0) 0)))

(stratum (g t)
         (λ (x)
           (sum y (* (rel V y)
                     (t x y)))))

(assert (forall (list x y z) (eq? (&& (E x z) (E y z)) (E x z))))

(assert (forall (list x y z) (= (* (I (E x y)) (I (T y z)))
                                (* (I (T x y)) (I (E y z))))))

(verify (assert (= (sum z (I (E z y))) (sum z (* (I (E z y)) (I (E x y)))))))

;; (optimize)

;; T(M,M2):- ∃[M1.T(M,M1),E(M1,M2)].
;; S(M,M2)=sum[M1:S(M,M1)*T(M1,M2)].
;; Key constraint on M2.

;; (sum z (* (I (rel E x z)) (S z)))

;; (sum z (* (I (rel E x z)) (sum y (* (rel V y) (* (I (rel E z y))  (I (rel T z y)))))))
