#lang rosette
(require "core/lib.rkt")
(require rosette/lib/angelic) ; provides `choose*`

;; HACK shadowing D
(decl rel D (~> id? id? int?))
(decl rel E (~> id? id? bool?))
(decl rel sigma (~> id? id? int?))

(decl var s t u v id?)

(idb (sig s t) `(rel sigma ,s ,t))

(def (delta s v t)
  (* (* (* (rel sigma s v)
           (rel sigma v t))
        (I (= (rel D s t) (+ (rel D s v) (rel D v t)))))
     (inv (rel sigma s t))))

(assert (forall (list s t u) (<= (D s u) (+ (D s t) (D t u)))))
(assert (forall (list s t) (<=> (E s t) (= 1 (D s t)))))
(assert (forall (list s t) (<=> (E s t) (= 1 (sigma s t)))))

(assert (= (* (inv (sigma s u)) (sigma s u)) 1))
(assert (= (* (inv (sigma s t)) (sigma s t)) 1))

(stratum (f sig)
         (λ (v t) (+
            (I (rel E v t))
            (sum u
                 (* (* (sig u t) (I (rel E v u)))
                    (I (= (rel D v t) (+ 1 (rel D u t)))))))))

(stratum (g sig)
         (λ (s v)
           (sum t
                (* (I (= (rel D s t) (+ (rel D s v) (rel D v t))))
                   (div (* (rel sigma s v) (sig v t))
                        (rel sigma s t))))))

;; (define (pick ts)
;;     (let ([vss (apply cartesian-product (map (curry hash-ref type->var) ts))])
;;       (apply choose* (filter (negate check-duplicates) vss))))

;; (define (??factor d)
;;   (if (= 0 d)
;;       (choose* (let ([r (choose* (lambda (vs) (op-rel sigma vs))
;;                                  (lambda (vs) (op-I (op-rel E vs))))])
;;                  (r (pick '(id? id?))))
;;                (op delta (pick '(id? id? id?))))
;;       ((choose* op-+ op-* op-/) (??factor (- d 1)) (??factor (- d 1)))))

;; (define sketch
;;   (op-+ (op-sum t (op-* (op delta (list s v t))
;;                         (??factor 0)))
;;         (op-sum (choose* s v t u)
;;                 (op-sum t
;;                         (op-* (op delta (list s v t)) (??factor 1))))))

;; (define M
;;   (synthesize
;;    #:forall (append (hash-values rel)
;;                     (hash-values var)
;;                     (hash-values fun)
;;                     (list sum inv))
;;    #:guarantee (assert (eq? (interpret sketch) p))))

;; (evaluate sketch M)

(optimize)

#;(+ (* (rel sigma s v)
        (sum t (* (I (= (rel D s t) (+ (rel D v t) (rel D s v))))
                  (* (I (rel E v t)) (* (rel sigma v t) (inv (rel sigma s t)))))))
     (sum u (* (I (rel E v u))
               (* (delta s v u) (S s u)))))

;; (sum t (+ (* (delta s v t) (I (rel E v t)))
;;           (sum u (* (delta s v u) (* (I (rel E v u)) (delta s u t))))))

#;(+ (sum t (* (I (E v t))
               (delta s v t)))
     (sum u (sum t (* (I (E v t))
                      (* (delta s v t)
                         (delta (s t u)))))))
