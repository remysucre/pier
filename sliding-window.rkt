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
  (- (rule-prefix t) (rule-prefix (- t k)) #;(* (rule-prefix (- t k)) (I (> k 0)))))


;; g(R)
(define (S t k)
  (- (prefix t) (prefix (- t k)) #;(* (prefix (- t k)) (I (> k 0)))))

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

(define sol (verify (assert (= (rule-S t k) (rule-S-opt t k)))))

;; (evaluate (rule-S t k) sol)
;; (evaluate (rule-S-opt t k) sol)
sol
