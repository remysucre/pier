#lang rosette
(require "core/lib.rkt")
(require rosette/lib/angelic) ; provides `choose*`
(require rosette/lib/synthax) ; provides `choose*`

;; HACK shadowing D
(decl rel D (~> id? id? int?))
(decl rel E (~> id? id? bool?))
(decl rel sigma (~> id? id? int?))

(decl var s t u v id?)

(idb (sig s t) `(rel sigma ,s ,t))

(def (delta s v t)
  (div (* (* (rel sigma s v)
           (rel sigma v t))
        (I (= (rel D s t) (+ (rel D s v) (rel D v t)))))
       (rel sigma s t)))

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

(define ??s (choose* s t u v))
(define ??v (choose* s t u v))
(define ??t (choose* s t u v))
(define (??var) (choose* s t u v))

(define (??base) (choose* (op-I (op-rel E (list (??var) (??var))))
                          (op-rel sigma (list (??var) (??var)))
                          (op delta (list (??var) (??var) (??var)))))
(define ??term-1 (op-* (??base) (??base)))

(define p
  (op-+ (op-sum t (op-* (op-I (op-eq? (op-rel D (list s t)) (op-+ (op-rel D (list s v)) (op-rel D (list v t)))))
                        (op-* (op-inv (op-rel sigma (list s t))) (op-* (op-rel sigma (list s v)) (op-I (op-rel E (list v t)))))))
        (op-sum
         t (op-sum
            u (op-* (op-* (op-I (op-rel E (list v u))) (??base))
                    (op-* (op-I (op-eq? (op-rel D (list ??s ??t))
                                        (op-+ (op-rel D (list ??s ??v)) (op-rel D (list ??v ??t)))))
                          (op-* (op-* (op-rel sigma (list ??s ??v))
                                      (op-rel sigma (list ??v ??t)))
                                (op-inv (op-rel sigma (list ??s ??t))))))))))

(define g-f-r (exp->struct (normalize ((g (f sig)) 's 'v)) symbol->var symbol->rel symbol->fun))

(define M
  (synthesize
   #:forall (append (hash-values symbol->rel)
                    (hash-values symbol->var)
                    #;(hash-values symbol->fun)
                    (list sum inv))
   #:guarantee (assert (eq? (interpret p) (interpret g-f-r)))))

(evaluate p M)
