#lang rosette
(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provides `destruct`

(define (I b) (if b 1 0))

;; TODO need to normalize

;; v[t] = SUM_w v(t,w) * w * 1_t>=1
(define (vec-get v t)
  (s-sum w
         (* w
            (I (&& (v t w)
                   (<= 1 t))))))

(define-symbolic s-sum (~> integer? integer? integer?))
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))
(define-symbolic j w integer?)

;; f(R)
(define (rule-R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))


;; g(f(R))
(define (rule-S t)
  (s-sum j
         (s-sum w
                (* w
                   (I (&& (rule-R t j w)
                          (<= 1 j)
                          (<= j t)))))))

;; g(R)
(define (S t)
  (s-sum j
      (s-sum w
             (* w
                (I (&& (R t j w)
                       (<= 1 j)
                       (<= j t)))))))

;; h(g(R)) (h to be synthesized)
(define (rule-S-opt t)
  (+ (S (- t 1))
     (vec-get v t)))

(define-symbolic t integer?)

;; Necessary for bounded verification
(assert (<= 0 t))

;; g.f = h.g
(verify (assert (= (rule-S t) (rule-S-opt t))))

(struct sub (left right) #:transparent)
(struct plus (left right) #:transparent)
(struct v-get (left right) #:transparent)
(struct rel-S (t) #:transparent)

; Interpreter for our DSL.
; We just recurse on the program's syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(sub a b)  (- (interpret a) (interpret b))]
    [(v-get a b)  (vec-get (interpret a) (interpret b))]
    [(rel-S t)  (S (interpret t))]
    [_ p]))

;; Grammar of semirings

;; op := + | -
(define (??op) (choose* plus sub))

;; vec := v
(define (??vec) (choose* v))

;; var := t
(define (??var) (choose* t))

;; atom := var | number
(define (??atom) (choose* (??var) (??)))

;; expr := atom | (op expr expr)
(define (??expr depth)
  (if (= depth 0)
      (choose* (??atom))
      (choose* (??atom)
               ((??op) (??expr (- depth 1))
                       (??expr (- depth 1))))))

;; term := (v-get v expr) | (op term term) | (S expr)
(define (??term depth)
  (if (= depth 1)
      (choose* (v-get v (??expr (- depth 1)))
               (rel-S (??expr (- depth 1))))
      (choose* (v-get v (??expr (- depth 1)))
               (rel-S (??expr (- depth 1)))
               ((??op) (??term (- depth 1))
                       (??term (- depth 1))))))

(define sketch (??term 3))

(define M
  (synthesize
   #:forall (list s-sum v R t)
   #:guarantee (assert (= (interpret sketch) (rule-S t)))))

(evaluate sketch M)
