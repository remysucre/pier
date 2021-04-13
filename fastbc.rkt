#lang rosette
(require "core/lib.rkt")
(require rosette/lib/angelic) ; provides `choose*`

;; HACK shadowing D
(decl rel D (~> id? id? int?))
(decl rel E (~> id? id? bool?))
(decl rel sigma (~> id? id? int?))

(decl var s t u v id?)

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

(define g (op-sum t (op delta (list s v t))))

(define (pick ts)
    (let ([vss (apply cartesian-product (map (curry hash-ref type->var) ts))])
      (apply choose* (filter (negate check-duplicates) vss))))

(define (??factor d)
  (if (= 0 d)
      (choose* (let ([r (choose* (lambda (vs) (op-rel sigma vs))
                                 (lambda (vs) (op-I (op-rel E vs))))])
                 (r (pick '(id? id?))))
               (op delta (pick '(id? id? id?))))
      ((choose* op-+ op-* op-/) (??factor (- d 1)) (??factor (- d 1)))))

(define sketch
  (op-+ (op-sum t (op-* (op delta (list s v t))
                        (??factor 0)))
        (op-sum (choose* s v t u)
                (op-sum t
                        (op-* (op delta (list s v t)) (??factor 1))))))

(define M
  (synthesize
   #:forall (append (hash-values symbol->rel)
                    (hash-values symbol->var)
                    (hash-values symbol->fun)
                    (list sum inv))
   #:guarantee (assert (eq? (interpret sketch) p))))

(evaluate sketch M)

;; (optimize p g)
