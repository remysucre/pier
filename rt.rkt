#lang rosette
(require "core/lib.rkt")
(require rosette/lib/angelic) ; provides `choose*`

(decl rel R (~> int? int? int? bool?))
(decl rel v (~> int? int? bool?))
(decl var t j w int?)

(idb (r x y w) `(I (rel R ,x ,y ,w)))

(def (vec-get j w t)
  (sum j
       (sum w
            (* w (* (I (rel v j w))
                    (* (I (= j t))
                       (I (<= 1 t))))))))

;; R(t,j,w):-v(j,w),t=j.
;; R(t,j,w):-R(t-1,j,w),1<=j<t.
(stratum (f r)
     (λ (t j w)
       (+ (* (I (rel v j w)) (I (= t j)))
          (* (r (- t 1) j w)
             (* (I (<= 1 (- t 1)))
                (I (<= j (- t 1))))))))

;; P[t]=sum[j,w:R(t,j,w)*w].
(stratum (g r)
     (λ (t)
       (sum j
            (sum w
                 (* (* (r t j w) w)
                    (* (I (<= 1 j)) (I (<= j t))))))))

(hash-update! type->var 'int? (curry cons (op-- t 1)))

(define ??t-1 (choose* t j (op-- t 1)))
(define ??j (choose* t j (op-- t 1)))

(define sketch (op-+ (op-sum
       j (op-sum
          w (op-* (op-I (op-eq? j t))
                  (op-* (op-I (op-leq 1 j))
                        (op-* (op-I (op-leq j t))
                              (op-* w (op-I (op-rel v (list j w)))))))))
      (op-sum
       j (op-sum
          w (op-* (op-* w (op-I (op-rel R (list ??t-1 j w))))
                  (op-* (op-I (op-leq 1 j))
                        (op-I (op-leq j ??t-1))))))))

(define g-f-r (exp->struct (normalize ((g (f r)) 't)) symbol->var symbol->rel symbol->fun))

(define M
  (synthesize
   #:forall (append (hash-values symbol->rel)
                    (hash-values symbol->var)
                    #;(hash-values symbol->fun)
                    (list sum inv))
   #:guarantee (assert (eq? (interpret sketch) (interpret g-f-r)))))
(evaluate sketch M)

;; (optimize)

;; (+ (vec-get j w t) (S (- t 1)))
