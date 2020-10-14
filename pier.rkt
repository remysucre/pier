#lang rosette/safe

;; b is a lowerbound of f
(define (lb w b f leq)
  (forall (list w) (leq b (f w))))

;; gb is a glb of f
(define (glb b w gb f leq)
  (&& (lb w gb f leq)
      (forall (list b)
          (=> (lb w b f leq)
              (leq b gb)))))

(define (f n) (+ (abs n) 1))

;; (define-symbolic s-min integer?)
;; (define-symbolic n b integer?)

;; (solve (assert (glb b n s-min f <=)))

;; (define (to-int b)
;;   (if b 1 0))

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

;; TODO replace define-symbolic with a define-symbolic*
;; inside glb?
(define (rule-R R E x z w)
  (begin
    (define (f0 y w1 w2)
      (&& (R x y w1)
          (&& (E y z w2)
              (= w (* w1 w2)))))
    (define (f1 y w1)
      (begin
        (define-symbolic w2 integer?)
        (define-symbolic any-w2 lb-w2 boolean?)
        (assert (glb lb-w2 w2 any-w2 ((curry f0) y w1) =>))
        any-w2))
    (define (f2 y)
      (begin
        (define-symbolic any-w1 w1 lb-w1 boolean?)
        (assert (glb lb-w1 w1 any-w1 ((curry f1) y) =>))
        any-w1))
    (define (f3)
      (begin
        (define-symbolic any-y y lb-y boolean?)
        (assert (glb lb-y y any-y f2 =>))
        any-y))
    ;; (|| (E x z w) (f3))
    (define-symbolic y w1 integer?)
    ;; (|| (E x z w) (f1 y w1))
    (f1 y w1)
    )
)

(clear-asserts!)

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x z w integer?)
(solve (assert (rule-R R E x z w)))

;; (rule-R (lambda (x y z) #t) (lambda (x y z) #t) 1 2 3)

;; (define (rule-S R E x z)
;;   (s-min
;;    (lambda (w) (* (to-int (rule-R R E x z w)) w))))

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
