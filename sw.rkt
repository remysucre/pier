#lang rosette
(require "core/lib.rkt")
(require rosette/lib/angelic) ; provides `choose*`
(require rosette/lib/synthax) ; provides `choose*`

(decl rel R (~> id? id? int? bool?))
(decl rel v (~> id? int? bool?))
(decl var t j k id?)
(decl var w int?)

(idb (r x y w) `(I (rel R ,x ,y ,w)))

(def (vec-get j w t)
  (sum j
       (sum w
            (* w (* (I (rel v j w))
                    (* (I (= j t))
                       (I (<= 1 j))))))))

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
     (λ (t k)
       (- (sum j
            (sum w
                 (* (* (r t j w) w)
                    (* (I (<= 1 j)) (I (<= j t))))))
          (sum j
            (sum w
                 (* (* (r (- t k) j w) w)
                    (* (I (<= 1 j)) (I (<= j (- t k))))))))))

(hash-update! type->var 'id? (curry cons (op-- t 1)))
(hash-update! type->var 'id? (curry cons (op-- t k)))
;; (hash-update! type->var 'int? (curry cons 0))

;; (optimize)

;; (+ (- (vec-get j w t) (vec-get j w (- t k))) (S (- t 1) k))
#;(- (+ (sum j (sum w (* (I (<= j t)) (* (I (<= 1 j)) (* (I (= j t)) (* w (I (rel v j w))))))))
      (sum j (sum w (* (I (<= j t)) (* (I (<= 1 j)) (* w (* (I (<= 1 (- t 1))) (* (I (rel R (- t 1) j w)) (I (<= j (- t 1)))))))))))
   (+ (sum j (sum w (* (I (<= 1 j)) (* (I (<= j (- t k))) (* (I (= j (- t k))) (* w (I (rel v j w))))))))
      (sum j (sum w (* (I (<= 1 j)) (* (I (<= j (- t k))) (* (I (<= 1 (- (- t k) 1)))
                                                             (* (I (rel R (- (- t k) 1) j w))
                                                                (* w (I (<= j (- (- t k) 1))))))))))))

#;(+ (+ (sum j (sum w (* (I (<= j t)) (* (I (<= 1 j)) (* (I (= j t)) (* w (I (rel v j w))))))))
      (sum j (sum w (* (I (<= j t)) (* (I (<= 1 j)) (* w (* (I (<= 1 (+ t -1))) (* (I (rel R (+ t -1) j w)) (I (<= j (+ t -1)))))))))))
   (+ (sum j (sum w (* -1 (* (I (<= 1 j)) (* (I (<= j (+ t (* -1 k)))) (* (I (= j (+ t (* -1 k)))) (* w (I (rel v j w)))))))))
      (sum j (sum w (* -1 (* (I (<= 1 j)) (* (I (<= j (+ t (* -1 k))))
                                             (* (I (<= 1 (+ (+ t (* -1 k)) -1)))
                                                (* (I (rel R (+ (+ t (* -1 k)) -1) j w))
                                                   (* w (I (<= j (+ (+ t (* -1 k)) -1)))))))))))))

#;(define sketch
  (op-+
   (op-+
    (op-sum
     j (op-sum
        w (op-* (op-I (op-leq j t))
                (op-* (op-I (op-leq 1 j))
                      (op-* (op-I (op-eq? j t))
                            (op-* w
                                  (op-I (op-rel v (list j w)))))))))
    (op-sum
     j (op-sum
        w (op-* (op-I (op-leq j t))
                (op-* (op-I (op-leq 1 j))
                      ;; here
                      (op-* w
                            (op-* (op-I (op-leq 1 (op-+ t -1)))
                                  (op-* (op-I (op-rel R (list (op-+ t -1) j w)))
                                        (op-I (op-leq j (op-+ t -1)))))))))))
   (op-+
    (op-sum
     j (op-sum
        w (op-* -1
                (op-* (op-I (op-leq 1 j))
                      (op-* (op-I (op-leq j (op-+ t (op-* -1 k))))
                            (op-* (op-I (op-eq? j (op-+ t (op-* -1 k))))
                                  (op-* w (op-I (op-rel v (list j w))))))))))
    (op-sum
     j (op-sum
        w (op-* -1 (op-* (op-I (op-leq 1 j))
                         (op-* (op-I (op-leq j (op-+ t (op-* -1 k))))
                               ;; here
                               (op-* (op-I (op-leq 1 (op-+ (op-+ t (op-* -1 k)) -1)))
                                     (op-* (op-I (op-rel R (list (op-+ (op-+ t (op-* -1 k)) -1) j w)))
                                           (op-* w
                                                 (op-I (op-leq j (op-+ (op-+ t (op-* -1 k)) -1))))))))))))))

(define ??t-1 (choose* t j k (op-- t 1) (op-- t k)))
(define ??j (choose* t j k (op-- t 1) (op-- t k)))
(define ??term-0 (choose* w 0 1 -1))

(define sketch
  (op-+
   (op-+
    (op-sum
     j (op-sum
        w (op-* (op-I (op-leq j t))
                (op-* (op-I (op-leq 1 j))
                      (op-* (op-I (op-eq? j t))
                            (op-* w
                                  (op-I (op-rel v (list j w)))))))))
    (op-sum
     j (op-sum
        w (op-* ??term-0
                (op-* w
                      (op-* (op-I (op-leq 1 ??j))
                            (op-* (op-I (op-rel R (list ??t-1 ??j w)))
                                  (op-I (op-leq ??j ??t-1)))))))))
   (op-+
    (op-sum
     j (op-sum
        w (op-* -1
                (op-* (op-I (op-leq 1 j))
                      (op-* (op-I (op-leq j (op-+ t (op-* -1 k))))
                            (op-* (op-I (op-eq? j (op-+ t (op-* -1 k))))
                                  (op-* w (op-I (op-rel v (list j w))))))))))
    (op-sum
     j (op-sum
        w (op-* -1 (op-* ??term-0
                         (op-* (op-I (op-leq 1 ??j))
                               (op-* (op-I (op-rel R (list (op-+ ??t-1 (op-* -1 k)) ??j w)))
                                     (op-* w
                                           (op-I (op-leq ??j (op-+ ??t-1 (op-* -1 k))))))))))))))

;; (verify (assert (= (interpret sketch) (interpret (exp->struct (normalize ((g (f r)) 't 'k)) symbol->var symbol->rel symbol->fun)))))
(define g-f-r (interpret (exp->struct (normalize ((g (f r)) 't 'k)) symbol->var symbol->rel symbol->fun)))

(define M
  (synthesize
   #:forall (append (hash-values symbol->rel)
                    (hash-values symbol->var)
                    #;(hash-values symbol->fun)
                    (list sum inv))
   #:guarantee (assert (eq? (interpret sketch) g-f-r))))
(evaluate sketch M)
