#lang rosette

(require rosette/lib/synthax)

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

(verify (assert (= (rule-S t k) (rule-S-opt t k))))

;; Grammar of semirings
;; op := + | - | vec-get
;; terminal := v | t | k | number
;; semiring := (op semiring semiring) | (S semiring semiring) | terminal
(define-synthax (semiring t k depth)
  #:base (choose v t k (??))
  #:else (choose v t k (??)
                 ((choose + -)
                  (semiring t k (- depth 1))
                  (semiring t k (- depth 1)))
                 (vec-get v (choose t k (??)
                                    ((choose + -) (choose t k (??))
                                                  (choose t k (??)))))
                 (S (choose t k (??)
                            ((choose + -) (choose t k (??))
                                          (choose t k (??))))
                    (choose t k (??)
                            ((choose + -) (choose t k (??))
                                          (choose t k (??)))))))

(define (optimized t k) (semiring t k 3))

(define OPT
  (synthesize
   #:forall (list v R t k)
   #:guarantee (assert (= (optimized t k) (rule-S t k)))))

(print-forms OPT)
