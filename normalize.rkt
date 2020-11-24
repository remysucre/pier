#lang rosette

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provides `destruct`

(define-symbolic sigma (~> integer? integer? integer?))
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))
(define-symbolic t j w integer?)

(assert (> t 0))

(define (I b) (if b 1 0))

(define (vec-get v t)
  (agg j
       (agg w
         (mul w
              (mul
               (mul (I (= j t)) (I (<= 1 t)))
               (I (v j w))
               )))))

;; f(R)
(define (rule-R t j w)
  (plus (mul (I (v j w)) (I (= t j)))
        (mul (I (R (- t 1) j w))
             (mul (I (< j t))
                  (I (> t 1))))))


;; g(f(R))
(define (rule-S t)
  (agg j
       (agg w
            (mul (mul (rule-R t j w) w)
                 (mul (I (<= 1 j))
                      (I (<= j t)))))))

;; g(R)
(define (S t)
  (agg j
      (agg w
           (mul
            (mul w (I (R t j w)))
            (mul (I (<= 1 j))
                 (I (<= j t)))))))

;; h(g(R)) (h to be synthesized)
(define (rule-S-opt t)
  (plus (S (- t 1))
     (vec-get v t)))

(struct sub (left right) #:transparent)
(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct agg (left right) #:transparent)

(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)  (* (interpret a) (interpret b))]
    [(sub a b)  (- (interpret a) (interpret b))]
    [(agg i e)  (sigma (interpret i) (interpret e))]
    [_ p]))

(define (normalize p)
  (match p
    [(mul a b) (let ([x (normalize a)]
                     [y (normalize b)])
                 (match x
                   [(plus l r) (plus (normalize (mul l y))
                                     (normalize (mul r y)))]
                   [_ (mul x y)]))]
    [(agg i e) (let ([x (normalize e)])
                 (match x
                   [(plus l r) (plus (normalize (agg i l))
                                     (normalize (agg i r)))]
                   [_ (agg i x)]))]
    [_ p]))

(verify (assert (= (interpret (normalize (rule-S-opt t)))
                   (interpret (normalize (rule-S t))))))
