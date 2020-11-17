#lang rosette
(require rosette/lib/synthax)

;; bound for model checking
(define N 4)
(define ws (list 0 1 2 3))

(define (s-sum f)
  (apply + (map f ws)))

(define (I b) (if b 1 0))

(define (vec-get v t)
  (s-sum
    (lambda (w)
      (* w
         (I (&& (v t w)
                (<= 1 t)))))))

;; f
(define (rule-R v R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))

;; g.f
(define (rule-S v R t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (rule-R v R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; g
(define (S v R t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; h.g (to be synthesized)
(define (rule-S-opt v R t)
  (+ (S v R (- t 1))
     (vec-get v t)))

(define-symbolic t integer?)
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

(assert (<= 0 t))
(assert (< t N))

;; g.f = h.g
(verify (assert (= (rule-S v R t) (rule-S-opt v R t))))

;; grammar of semirings
;; op := + | - | vec-get
;; terminal := v | t | number
;; semiring := (op semiring semiring) | (S semiring) | terminal
(define-synthax (semiring v R t depth)
  #:base (choose v t (??))
  #:else (choose v t (??)
                 ((choose + - vec-get)
                  (semiring v R t (- depth 1))
                  (semiring v R t (- depth 1)))
                 (S v R (semiring v R t (- depth 1)))))

(define (optimized v R t)
  (semiring v R t 3))

(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized v R t) (rule-S v R t)))))

(print-forms OPT)
