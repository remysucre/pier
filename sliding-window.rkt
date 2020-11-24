#lang rosette

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provides `destruct`

;; Bound for verification
(define N 4)
(define ws (range N))

(define (s-sum f) (apply + (map f ws)))

(define (I b) (if b 1 0))

;; v[t] = SUM_w v(t,w) * w * 1_t>=1
(define (vec-get v t)
  (s-sum
    (lambda (w)
      (* w
         (I (&& (v t w)
                (<= 1 t)))))))

(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

;; f(R)
(define (rule-R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))

(define (rule-prefix t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (rule-R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

(define (prefix t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; g(f(R))
(define (rule-S t k)
  (- (rule-prefix t) (rule-prefix (- t k))))


;; g(R)
(define (S t k)
  (- (prefix t) (prefix (- t k))))

;; h(g(R)) (h to be synthesized)
(define (rule-S-opt t k)
  (+ (S (- t 1) k)
     (- (vec-get v t)
        (vec-get v (- t k)))))

(define-symbolic t k integer?)

;; Necessary for bounded verification
(assert (<= 0 t))
(assert (< t N))

(assert (> k 0))

;; (verify (assert (= (rule-S t k) (rule-S-opt t k))))

(struct sub (left right) #:transparent)
(struct plus (left right) #:transparent)
(struct v-get (left right) #:transparent)
(struct rel-S (t k) #:transparent)

; Interpreter for our DSL.
; We just recurse on the program's syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(sub a b)  (- (interpret a) (interpret b))]
    [(v-get a b)  (vec-get (interpret a) (interpret b))]
    [(rel-S t k)  (S (interpret t) (interpret k))]
    [_ p]))

;; Grammar of semirings

;; op := + | -
(define (??op) (choose* plus sub))

;; vec := v
(define (??vec) (choose* v))

;; var := t | k
(define (??var) (choose* t k))

;; atom := var | number
(define (??atom) (choose* (??var) (??)))

;; expr := atom | (op expr expr)
(define (??expr depth)
  (if (= depth 0)
      (choose* (??atom))
      (choose* (??atom)
               ((??op) (??expr (- depth 1))
                       (??expr (- depth 1))))))

;; term := (v-get v expr) | (op term term) | (S expr expr)
;; depth cannot be less than 1
(define (??term t-depth e-depth)
  (if (= t-depth 1)
      (choose* (v-get v (??expr e-depth))
               (rel-S (??expr e-depth)
                      (??expr e-depth)))
      (choose* (v-get v (??expr e-depth))
               (rel-S (??expr e-depth)
                      (??expr e-depth))
               ((??op) (??term (- t-depth 1) e-depth)
                       (??term (- t-depth 1) e-depth)))))

(define sketch (??term 3 1))

(define M
  (synthesize
   #:forall (list v R t k)
   #:guarantee (assert (= (interpret sketch) (rule-S t k)))))

(evaluate sketch M)
