#lang rosette
(require "core/lib.rkt")

(decl rel R (~> int? int? int? bool?))
(decl rel v (~> int? int? bool?))
(decl var t j w int?)

(def (vec-get j w t)
  (sum j
       (sum w
            (* w (* (I (v j w))
                    (* (I (= j t))
                       (I (<= 1 t))))))))

;; R(t,j,w):-v(j,w),t=j.
;; R(t,j,w):-R(t-1,j,w),1<=j<t.
(rec (f R)
     (λ (t j w)
       (+ (* (I (v j w)) (I (= t j)))
          (* (I (R (- t 1) j w))
             (* (I (<= 1 (- t 1)))
                (I (<= j (- t 1))))))))

;; P[t]=sum[j,w:R(t,j,w)*w].
(ret (g R)
     (λ (t)
       (sum j
            (sum w
                 (* (* (R t j w) w)
                    (* (I (<= 1 j)) (I (<= j t))))))))

(hash-update! type->var 'int? (curry cons (op-- t 1)))

(optimize)

;; (+ (vec-get j w t) (S (- t 1)))
