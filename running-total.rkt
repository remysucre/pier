#lang rosette
(require rosette/lib/synthax)

;; bound for model checking
(define N 4)

(define (I b) (if b 1 0))

(define (s-sum f)
  (lambda (w) (apply + (map f w))))

(define (vec-get v t)
  ((s-sum
    (lambda (w)
      (* w
         (I (&& (v t w)
                (<= 1 t))))))
   ws))

;; f
(define (rule-R v R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))

;; g.f
(define (rule-S v R t js ws)
  ((s-sum
    (lambda (j)
      ((s-sum
        (lambda (w)
          (* w
             (I (&& (rule-R v R t j w)
                    (<= 1 j)
                    (<= j t))))))
       ws)))
   js))

;; g
(define (S v R t js ws)
  ((s-sum
    (lambda (j)
      ((s-sum
        (lambda (w)
          (* w
             (I (&& (R t j w)
                    (<= 1 j)
                    (<= j t))))))
       ws)))
   js))

;; h.g (to be synthesized)
(define (rule-S-opt v R t js ws)
  (+ (S v R (- t 1) js ws)
     (vec-get v t)))

(define-symbolic t integer?)
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

(define js (range N))
(define ws (range N))

(assert (<= 0 t))
(assert (< t N))

;; g.f = h.g
(verify (assert (= (rule-S v R t js ws) (rule-S-opt v R t js ws))))

;; grammar of semirings, with the hint to include S[t-1] in a terminal
(define-synthax (semiring v R t js ws depth)
  #:base (choose (S v R (- t 1) js ws)
                 v R js ws t 1)
  #:else (choose (S v R (- t 1) js ws)
                 v R js ws t 1
                 ((choose + - vec-get)
                  (semiring v R t js ws (- depth 1))
                  (semiring v R t js ws (- depth 1)))))

(define (optimized v R t js ws)
  (semiring v R t js ws 3))

(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized v R t js ws) (rule-S v R t js ws)))))

(print-forms OPT)
