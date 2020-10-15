#lang rosette/safe

;; b is an upperbound of f(w)
(define (ub w b f geq)
  (forall (list w) (geq b (f w))))

;; lb is a lub of f
(define (lub b w lb f geq)
  (&& (ub w lb f geq)
      (forall (list b)
          (=> (ub w b f geq)
              (geq b lb)))))

;; (define (f n) (+ (abs n) 1))

;; (define-symbolic s-min integer?)
;; (define-symbolic n b integer?)

;; (solve (assert (lub b n s-min f <=)))

#;(define (rule-R R E x z w)
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

(define (rule-R R E x z w)
  (begin
    (define (f0 y w1 w2)
      (&& (R x y w1)
          (&& (E y z w2)
              (= w (* w1 w2)))))
    (define (f1 y w1)
      (begin
        (define-symbolic w2 integer?)
        (define-symbolic any-w2 ub-w2 boolean?)
        (assert (lub ub-w2 w2 any-w2 ((curry f0) y w1) =>))
        any-w2))
    (define (f2 y)
      (begin
        (define-symbolic w1 integer?)
        (define-symbolic any-w1 ub-w1 boolean?)
        (assert (lub ub-w1 w1 any-w1 ((curry f1) y) =>))
        any-w1))
    (define (f3)
      (begin
        (define-symbolic y integer?)
        (define-symbolic any-y ub-y boolean?)
        (assert (lub ub-y y any-y f2 =>))
        any-y))
    (|| (E x z w) (f3))
    )
  )

;; (clear-asserts!)
;; (define-symbolic R E (~> integer? integer? integer? boolean?))
;; (define-symbolic x z w integer?)
;; (solve (assert (! (rule-R R E x z w))))

;; #f represents infinity
(define (to-trop b)
  (if b 0 #t))

(define (t+ x y)
  (if (number? x)
      (if (number? y)
          (min x y)
          x)
      y))

(define (t* x y)
  (if (number? x)
      (if (number? y)
          (+ x y)
          #t)
      #t))

(define (t>= x y)
  (if (number? x)
      (if (number? y)
          (<= x y)
          #f)
      #t))

;; b is an upperbound of f(w)
;; if f(w) is unbounded, inf is #t
(define (trop-ub w b f)
  (forall (list w)
          (if (boolean? (f w))
              #t
              (<= b (f w)))))

;; lb is a lub of f
;; if f is unbounded, inf is true
(define (trop-lub b w lb f)
  (&& (trop-ub w lb f)
      (forall (list b)
              (=> (ub w b f)
                  (<= b lb)))))

#;(define (rule-S R E x z)
  (s-min
   (lambda (w) (* (to-int (rule-R R E x z w)) w))))

(define (rule-S R E x z)
  (begin
    (define (f0 w)
      ;;(t* (to-trop (rule-R R E x z w)) w))
      #;(t* (to-trop (R x z w)) w)
      (t* (to-trop (R x z w)) w)
      )
    (define (f1)
      (define-symbolic w integer?)
      (define-symbolic inf boolean?)
      (define-symbolic min-w ub-w integer?)
      ;;(assert (trop-lub inf ub-w w min-w f0))
      (assert (trop-ub w min-w f0))
      (if inf inf min-w))
    (f1)
    )
  )

(define-symbolic x z integer?)
(define-symbolic R E (~> integer? integer? integer? boolean?))
;; (define-symbolic R (~> integer? integer? integer? boolean?))
;; (rule-S R E x z)
;; (solve (rule-S R E x z))
;; (solve (rule-S R E x z))
(solve (assert (= (rule-S R E x z) 5)))

;; (define (R x y z) (= z 3))
;; (define (E x y z) (= z 3))
;; (solve (assert (< (rule-S R E 1 2) 0)))

;; (assert (forall w (<= s-min (* (to-int (rule-R R E x z w)) w))))
;; (assert (forall z (=> (forall w (<= z (* (to-int (rule-R R E x z w)) w))) (>= s-min z))))
;; (assert (exists w (= s-min (* (to-int (rule-R R E x z w)) w))))


;; (define (rule-S-opt R E x z)
;;   (min (s-min
;;         (lambda (w)
;;           (* (to-int (E x z w)) w)))
;;        (s-min
;;         (lambda (y)
;;           (+ (rule-S R E x y)
;;              (s-min
;;               (lambda (w2)
;;                 (* (to-int (E y z w2)) w2))))))))

;; (define-symbolic x z integer?)
;; (define-symbolic R (~> integer? boolean?))
;; (define-symbolic E (~> integer? boolean?))
;; (define sol (verify (assert (= (rule-S-opt R E x z) (rule-S R E x z)))))
