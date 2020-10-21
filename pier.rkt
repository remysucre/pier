#lang rosette/safe

(define (ub b f geq)
  (begin
    (define-symbolic* w integer?)
    (forall (list w) (geq b (f w)))))

(define (lub lb f geq)
  (begin
    (define-symbolic* b boolean?)
    (&& (ub lb f geq)
      (forall (list b)
          (=> (ub b f geq)
              (geq b lb))))))

;; (define (f n) (> n 0))
;; (define-symbolic s-any boolean?)
;; (solve (assert (lub s-any f =>)))

#;(define (rule-R R E x z w)
  (|| (E x z w)
      (s-any
       (lambda (y)
         (s-any
          (lambda (w1)
            (s-any
             (lambda (w2)
               (&& (R x y w1)
                   (E y z w2)
                   (= w (+ w1 w2)))))))))))

(define (rule-R R E x z w)
  (begin
    (define (f0 y w1 w2)
      (&& (R x y w1)
          (E y z w2)
          (= w (+ w1 w2))))
    (define (f1 y w1)
      (begin
        (define-symbolic any-w2 boolean?)
        (assert (lub any-w2 ((curry f0) y w1) =>))
        any-w2))
    (define (f2 y)
      (begin
        (define-symbolic any-w1 boolean?)
        (assert (lub any-w1 ((curry f1) y) =>))
        any-w1))
    (define (f3)
      (begin
        (define-symbolic any-y boolean?)
        (assert (lub any-y f2 =>))
        any-y))
    (|| (E x z w) (f3))))

;; (define-symbolic R E (~> integer? integer? integer? boolean?))
;; (define-symbolic x z w integer?)
;; (solve (assert (rule-R R E x z w)))

;; #f represents infinity

(struct trop (i n) #:transparent)

;; TODO try (trop #t (assert #f))
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
  (begin
    (define-symbolic* w integer?)
    (forall (list w) (t>= b (f w)))))

;; lb is a lub of f

(define (trop-lub lb f)
  (begin
    (define-symbolic* n integer?)
    (define-symbolic* i boolean?)
    (&& (trop-ub lb f)
        (forall (list i n)
                (=> (trop-ub (trop i n) f)
                    (t>= (trop i n) lb))))))

(define (s-abs n) (trop #f (abs n)))

(verify (assert (trop-ub (trop #f 0) s-abs)))
(verify (assert (trop-ub (trop #f -1) s-abs)))
(verify (assert (! (trop-ub (trop #f 1) s-abs))))

(verify (assert (trop-lub (trop #f 0) s-abs)))
(verify (assert (! (trop-lub (trop #f -1) s-abs))))
(verify (assert (! (trop-lub (trop #f 1) s-abs))))

;; (define (rule-S R E x z)
;;   (s-min
;;    (lambda (w) (* (to-int (rule-R R E x z w)) w))))

(define (rule-S R E x z)
  (begin
    (define (f0 w)
      (t* (to-trop (rule-R R E x z w))
          (trop #f w)))
    (define (f1)
      (define-symbolic min-w integer?)
      (assert (trop-lub (trop #f min-w) f0))
      min-w)
    (f1)))

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
