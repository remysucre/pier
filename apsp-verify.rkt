#lang rosette/safe

(define (ub-b b f)
  (define-symbolic* w integer?)
  (forall (list w) (=> (f w) b)))

(define (test-ub)
  (define (f n) (> n 0))
  (define (g n) (> n n))
  (define (h n) (= n n))
  (define (m n) (list? (member n (list 2 4 5))))

  (list
  (verify (assert (ub-b #t f)))
  (verify (assert (! (ub-b #f f))))
  (verify (assert (ub-b #t h)))
  (verify (assert (! (ub-b #f h))))
  (verify (assert (ub-b #f g)))
  (verify (assert (ub-b #t g)))
  (verify (assert (ub-b #t m)))
  (verify (assert (! (ub-b #f m))))))

(define (lub-b lb f)
  (define-symbolic* b boolean?)
  (&&
   (ub-b lb f)
   (forall (list b)
           (=> (ub-b b f)
               (=> lb b)))))

(define (test-lub)
  (define (f n) (> n 0))
  (define (g n) (> n n))
  (define (h n) (= n n))
  (define (m n) (list? (member n (list 2 4 5))))

  (list
  (verify (assert (lub-b #t f)))
  (verify (assert (! (lub-b #f f))))
  (verify (assert (lub-b #t h)))
  (verify (assert (! (lub-b #f h))))
  (verify (assert (lub-b #f g)))
  (verify (assert (! (lub-b #t g))))
  (verify (assert (lub-b #t m)))
  (verify (assert (! (lub-b #f m))))))

(struct trop (i n) #:transparent)

(define (to-trop b)
  (if b (trop #f 0) (trop #t 1)))

(assert (eq? (trop #f 0) (to-trop #t)))
(assert (eq? (trop #t 1) (to-trop #f)))

;; min
(define (t+ x y)
  (if (trop-i x)
      y
      (if (trop-i y)
          x
          (trop #f (min (trop-n x)
                        (trop-n y))))))

(assert (eq? (t+ (trop #t 1) (trop #t 1)) (trop #t 1)))
(assert (eq? (t+ (trop #t 1) (trop #f 1)) (trop #f 1)))
(assert (eq? (t+ (trop #f 1) (trop #t 1)) (trop #f 1)))
(assert (eq? (t+ (trop #f 1) (trop #f 2)) (trop #f 1)))

;; +
(define (t* x y)
  (if (trop-i x)
      (trop #t 1)
      (if (trop-i y)
          (trop #t 1)
          (trop #f (+ (trop-n x)
                      (trop-n y))))))

(assert (eq? (t* (trop #t 1) (trop #t 1)) (trop #t 1)))
(assert (eq? (t* (trop #t 1) (trop #f 1)) (trop #t 1)))
(assert (eq? (t* (trop #f 1) (trop #t 1)) (trop #t 1)))
(assert (eq? (t* (trop #f 1) (trop #f 2)) (trop #f 3)))

;; <=
(define (t>= x y)
  (if (trop-i y)
      #t
      (if (trop-i x)
          #f
          (<= (trop-n x) (trop-n y)))))

(assert (t>= (trop #f 5) (trop #t 1)))
(assert (! (t>= (trop #t 1) (trop #f 5))))
(assert (t>= (trop #f 5) (trop #f 6)))
(assert (t>= (trop #t 5) (trop #t 1)))

;; b is an upperbound of f(w)

(define (trop-ub b f)
  (define-symbolic* w integer?)
  (forall (list w) (t>= b (f w))))

(define (test-trop-ub)
  (define (f n) (trop #f n))
  (define (g n) (trop #f (abs n)))
  (define (h n) (trop #f (* n n)))
  (define (i n) (trop #t 1))
  (define (m n) (to-trop (list? (member n (list 2 4 5)))))

  (list
  (verify (assert (! (trop-ub (trop #f 1) f))))
  (verify (assert (! (trop-ub (trop #t 1) f))))

  (verify (assert (! (trop-ub (trop #t 1) g))))
  (verify (assert (! (trop-ub (trop #f 1) g))))
  (verify (assert (trop-ub (trop #f 0) g)))
  (verify (assert (trop-ub (trop #f -1) g)))

  (verify (assert (! (trop-ub (trop #t 1) h))))
  (verify (assert (! (trop-ub (trop #f 1) h))))
  (verify (assert (trop-ub (trop #f 0) h)))
  (verify (assert (trop-ub (trop #f -1) h)))

  (verify (assert (trop-ub (trop #t 1) i)))
  (verify (assert (trop-ub (trop #f 1) i)))
  (verify (assert (trop-ub (trop #f 0) i)))
  (verify (assert (trop-ub (trop #f -1) i)))

  (verify (assert (trop-ub (trop #t 1) i)))
  (verify (assert (trop-ub (trop #f 2) i)))
  (verify (assert (trop-ub (trop #f 3) i)))
  (verify (assert (trop-ub (trop #f -1) i)))))

;; lb is a lub of f

(define (trop-lub lb f)
    (define-symbolic* n integer?)
    (define-symbolic* i boolean?)
    (&& (trop-ub lb f)
        (forall (list i n)
                (=> (trop-ub (trop i n) f)
                    (t>= (trop i n) lb)))))

(define (test-trop-lub)
  (define (f n) (trop #f n))
  (define (g n) (trop #f (abs n)))
  (define (h n) (trop #f (* n n)))
  (define (i n) (trop #t 1))
  (define (m n) (to-trop (list? (member n (list 2 4 5)))))

  (list
  (verify (assert (! (trop-lub (trop #f 1) f))))
  (verify (assert (! (trop-lub (trop #t 1) f))))

  (verify (assert (! (trop-lub (trop #t 1) g))))
  (verify (assert (! (trop-lub (trop #f 1) g))))
  (verify (assert (trop-lub (trop #f 0) g)))
  (verify (assert (! (trop-lub (trop #f -1) g))))

  (verify (assert (! (trop-lub (trop #t 1) h))))
  (verify (assert (! (trop-lub (trop #f 1) h))))
  (verify (assert (trop-lub (trop #f 0) h)))
  (verify (assert (! (trop-lub (trop #f -1) h))))

  (verify (assert (trop-lub (trop #t 1) i)))
  (verify (assert (! (trop-lub (trop #f 1) i))))
  (verify (assert (! (trop-lub (trop #f 0) i))))
  (verify (assert (! (trop-lub (trop #f -1) i))))

  (verify (assert (trop-lub (trop #t 1) i)))
  (verify (assert (! (trop-lub (trop #f 2) i))))
  (verify (assert (! (trop-lub (trop #f 3) i))))
  (verify (assert (! (trop-lub (trop #f -1) i))))))

;; E(x,y) * sum_w R(x,y,w) = sum_w E(x,y) * R(x,y,w)

;; sum_w S(x,y,w) + (R(x,y,w) = sum_w S(x,y,w) + sum_w R(x,y,w)

(define (E*s_wR E R)
  (define (f x y)
    (define-symbolic* min-w integer?)
    (assert (trop-lub (trop #f min-w)
                      (lambda (w) (trop #f (R x y w)))))
    (+ (E x y)
       min-w))
  f)

(define (test-s1)
  (define (E x y) (+ x y))
  (define (R x y w) (+ x y (* (+ 2 w) w)))
  (define out ((E*s_wR E R) 4 2))
  (define sol (solve #t))
  (assert (= 30 (evaluate out sol))))

(define (s_wE*R E R)
  (define (f x y)
    (define (f0 w)
      (trop #f (+ (E x y) (R x y w))))
    (define-symbolic* min-w integer?)
    (assert (trop-lub (trop #f min-w) f0))
    min-w)
  f)

(define (test-s2)
  (define (E x y) (+ x y))
  (define (R x y w) (+ x y (* (+ 2 w) w)))
  (define out ((s_wE*R E R) 4 2))
  (define sol (solve #t))
  (assert (= 30 (evaluate out sol))))

(define (test-*sum)
  (define-symbolic E (~> integer? integer? integer?))
  (define-symbolic R (~> integer? integer? integer? integer?))
  (define-symbolic x y integer?)
  (define r1 ((E*s_wR E R) x y))
  (define r2 ((s_wE*R E R) x y))
  (verify (assert (= r1 r2))))

(define (s_wR+s_wS R S)
  (define (f x y)
    (define-symbolic* min-w1 min-w2 integer?)
    (assert (trop-lub (trop #f min-w1)
                      (lambda (w) (trop #f (R x y w)))))
    (assert (trop-lub (trop #f min-w2)
                      (lambda (w) (trop #f (S x y w)))))
    (min min-w1 min-w2))
  f)

(define (s_wR+S R S)
  (define (f x y)
    (define-symbolic* min-w integer?)
    (assert (trop-lub (trop #f min-w)
                      (lambda (w) (trop #f (min (S x y w)
                                                (R x y w))))))
    min-w)
  f)

(define (test-+sum)
  (define-symbolic R S (~> integer? integer? integer? integer?))
  (define-symbolic x y integer?)
  (define r1 ((s_wR+s_wS R S) x y))
  (define r2 ((s_wR+S R S) x y))
  (verify (assert (= r1 r2))))
