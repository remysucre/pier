#lang rosette/safe

(define (s-min f)
  (begin
    (define-symbolic x y z real?)
    (define sol
      (solve (begin (assert (forall (list y) (<= x (f y))))
                    (assert (= x (f z))))))
    (evaluate x sol)))

(verify (assert (= (s-min (lambda (y) (abs y))) 0)))

(define (s-any f)
  (begin
    (define-symbolic x boolean?)
    (define-symbolic y z integer?)
    (define sol
      (solve (begin (assert (forall (list y) (=> (f y) x)))
                    (assert (<=> x (f z))))))
    (evaluate x sol)))

(verify (assert (<=> (s-any (lambda (x) (> x 0))) #t)))
(verify (assert (<=> (s-any (lambda (x) (> x x))) #f)))

(define (to-int b)
  (if b 1 0))

(define (rule-R R E x z w)
  (|| (E x z w)
       (s-any
        (lambda (y)
          (s-any
           (lambda (w1)
             (s-any
              (lambda (w2)
                 (&& (R x y w1)
                     (&& (E y z w2)
                         (= w (* w1 w2))))))))))))

;; lower bound:
;; forall x. s-min <= f x
;; glub:
;; forall z. (forall x. z <= f x) => s-min >= z
;; alternatively:
;; exists x. s-min = f x

(define (rule-S R E x z)
  (s-min
   (lambda (w) (* (to-int (rule-R R E x z w)) w))))

(define (rule-S-opt R E x z)
  (min (s-min
        (lambda (w)
          (* (to-int (E x z w)) w)))
       (s-min
        (lambda (y)
          (+ (rule-S R E x y)
             (s-min
              (lambda (w2)
                (* (to-int (E y z w2)) w2))))))))

;; (define-symbolic x z integer?)
;; (define-symbolic R (~> integer? boolean?))
;; (define-symbolic E (~> integer? boolean?))
;; (define sol (verify (assert (= (rule-S-opt R E x z) (rule-S R E x z)))))
