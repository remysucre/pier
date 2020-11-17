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

(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

;; f
(define (rule-R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))

;; g.f
(define (rule-S t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (rule-R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; g
(define (S t)
  (s-sum
    (lambda (j)
      (s-sum
        (lambda (w)
          (* w
             (I (&& (R t j w)
                    (<= 1 j)
                    (<= j t)))))))))

;; h.g (to be synthesized)
(define (rule-S-opt t)
  (+ (S (- t 1))
     (vec-get v t)))

(define-symbolic t integer?)

(assert (<= 0 t))
(assert (< t N))

;; g.f = h.g
(verify (assert (= (rule-S t) (rule-S-opt t))))

;; grammar of semirings
;; op := + | - | vec-get
;; terminal := v | t | number
;; semiring := (op semiring semiring) | (S semiring) | terminal
(define-synthax (semiring v t depth)
  #:base (choose v t (??))
  #:else (choose v t (??)
                 ((choose + - vec-get)
                  (semiring v t (- depth 1))
                  (semiring v t (- depth 1)))
                 (S (semiring v t (- depth 1)))))

(define (optimized v t)
  (semiring v t 3))

(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized v t) (rule-S t)))))

(print-forms OPT)
