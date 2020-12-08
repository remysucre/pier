#lang rosette/safe

;; (define (ub b f leq)
;;     (define-symbolic* w integer?)
;;     (exists (list w) (f w)))

(define (lub f)
    ;; (define-symbolic* b boolean?)
    (define-symbolic* w integer?)
    (exists (list w) (f w))

    #;(&& (ub lb f leq)
        (=> (ub #t f leq) lb)
        #;(forall (list b)
                (=> (ub b f leq)
                    (leq lb b)))))

(struct trop (i n) #:transparent)

(define (to-trop b)
  (if b (trop #f 0) (trop #t 1)))

;; min
(define (t+ x y)
  (if (trop-i x)
      y
      (if (trop-i y)
          x
          (trop #f (min (trop-n x)
                        (trop-n y))))))

;; +
(define (t* x y)
  (if (trop-i x)
      (trop #t 1)
      (if (trop-i y)
          (trop #t 1)
          (trop #f (+ (trop-n x)
                      (trop-n y))))))

;; <=
(define (t>= x y)
  (if (trop-i y)
      #t
      (if (trop-i x)
          #f
          (<= (trop-n x) (trop-n y)))))

(assert (t>= (trop #f 5) (trop #t 1)))
(assert (t>= (trop #f 5) (trop #f 6)))
(assert (t>= (trop #t 5) (trop #t 1)))

;; b is an upperbound of f(w)

(define (trop-ub b f)
  (define-symbolic* w integer?)
  (forall (list w) (t>= b (f w))))

;; lb is a lub of f

(define (trop-lub lb f)
    (define-symbolic* n integer?)
    (define-symbolic* i boolean?)
    (&& (trop-ub lb f)
        (forall (list i n)
                (=> (trop-ub (trop i n) f)
                    (t>= (trop i n) lb)))))

(define (s-abs n) (trop #f (abs n)))

(define (rule-S R E x z)
  (define (f0 w)
    (t* (to-trop (rule-R R E x z w))
        (trop #f w)))
  (define (f1)
    (define-symbolic min-wS integer?)
    (assert (trop-lub (trop #f min-wS) f0))
    min-wS)
  (f1))

(define (S R x z)
    (define (f0 w)
      (t* (to-trop (R x z w))
          (trop #f w)))
    (define (f1)
      (define-symbolic min-w integer?)
      (assert (trop-lub (trop #f min-w) f0))
      min-w)
    (f1))

(define (rule-R R E x z w)
  (define (f0 y w1 w2)
    (&& (R x y w1)
        (E y z w2)
        (= w (+ w1 w2))))
  (define (f1 y w1)
    ;; (define-symbolic* any-w2 boolean?)
    ;; (assert (lub any-w2 ((curry f0) y w1) =>))
    #;any-w2
    (lub ((curry f0) y w1)))
  (define (f2 y)
    ;; (define-symbolic* any-w1 boolean?)
    ;; (assert (lub any-w1 ((curry f1) y) =>))
    ;;any-w1
    (lub ((curry f1) y)))
  (define (f3)
    ;; (define-symbolic* any-y boolean?)
    ;; (assert (lub any-y f2 =>))
    ;; any-y
    (lub f2))
  (|| (E x z w) (f3)))

;; (define (S-27 R E x z)
;;   (define (R-il R E x z w)
;;     (define (f0 y w1 w2)
;;       (&& (R x y w1)
;;           (E y z w2)
;;           (= w (+ w1 w2))))
;;     (define (f1 y w1)
;;       (lub ((curry f0) y w1)))
;;     (define (f2 y)
;;       (lub ((curry f1) y)))
;;     (define (f3)
;;       (lub f2))
;;     (|| (E x z w) (f3)))
;;   (define (f0 w)
;;     (t* (to-trop (R-il R E x z w))
;;         (trop #f w)))
;;   (define (f1)
;;     (define-symbolic min-wS integer?)
;;     (assert (trop-lub (trop #f min-wS) f0))
;;     min-wS)
;;   (f1))

(define (S-27 R E x z)
    (define (f0 w y w1 w2)
      (&& (R x y w1)
          (E y z w2)
          (= w (+ w1 w2))))
    (define (f1 w y w1)
      (lub ((curry f0) w y w1)))
    (define (f2 w y)
      (lub ((curry f1) w y)))
    (define (f3 w)
      (t* (to-trop (lub ((curry f2) w)))
          (trop #f w)))
    (define (f4)
      (define-symbolic* min-w+ integer?)
      (assert (trop-lub (trop #f min-w+) f3))
      min-w+)
    (define (s0 w)
      (t* (to-trop (E x z w))
          (trop #f w)))
    (define (s1)
      (define-symbolic* min-w integer?)
      (assert (trop-lub (trop #f min-w) s0))
      min-w)
    (min (s1) (f4)))

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

;; (define (rule-S-opt R E x z)
;;   (begin
;;     (define (f0 w)
;;       (t* (to-trop (E x z w))
;;           (trop #f w)))
;;     (define (f1)
;;       (define-symbolic min-w integer?)
;;       (assert (trop-lub (trop #f min-w) f0))
;;       min-w)
;;     (define (g0 y w2)
;;       (t* (to-trop (E y z w2))
;;           (trop #f w2)))
;;     (define (g1 y)
;;       (define-symbolic min-w2 integer?)
;;       (assert (trop-lub (trop #f min-w2) ((curry g0) y)))
;;       (t* (trop #f (S R x y))
;;           (trop #f min-w2)))
;;     (define (g2)
;;       (define-symbolic min-y integer?)
;;       (assert (trop-lub (trop #f min-y) g1))
;;       min-y)
;;     (min (f1) (g2))))

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x z integer?)

(define S-out (rule-S R E x z))
(define S-27-out (S-27 R E x z))


(verify (assert #f))

;; (verify (assert (! (= S-27-out S-out))))
;; (verify (assert (= S-27-out S-out)))