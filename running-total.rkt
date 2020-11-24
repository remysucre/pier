#lang rosette
(require rosette/lib/angelic
         rosette/lib/synthax)

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

;; g(f(R))
(define (rule-S t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (rule-R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; g(R)
(define (S t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; h(g(R)) (h to be synthesized)
(define (rule-S-opt t)
  (+ (S (- t 1))
     (vec-get v t)))

(define-symbolic t integer?)

;; Necessary for bounded verification
(assert (<= 0 t))
(assert (< t N))

;; g.f = h.g
(verify (assert (= (rule-S t) (rule-S-opt t))))

;; Grammar of semirings
;; op := + | - | vec-get
;; terminal := v | t | number
;; semiring := (op semiring semiring) | (S semiring) | terminal
;; (define-synthax (semiring t depth)
;;   #:base (choose v t (??))
;;   #:else (choose v t (??)
;;                  ((choose + - vec-get)
;;                   (semiring t (- depth 1))
;;                   (semiring t (- depth 1)))
;;                  (S (semiring t (- depth 1)))))

(define (??expr t depth)
  (if (= depth 0)
      (choose* t (??))
      (choose* t (??)
              ((choose* + -)
               (??expr (- depth 1))
               (??expr (- depth 1))))))

(define (??term t depth)
  (if (= depth 0)
      (??expr t depth)
      (choose* (??expr t depth)
               (vec-get v (??expr t (- depth 1)))
               ((choose* + -) (??term t (- depth 1))
                              (??term t (- depth 1)))
               (S (??expr t (- depth 1))))))

(define (optimized t) (??expr t 0))

#;(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized t) (rule-S t)))))

(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized t) t))))

(print-forms OPT)
