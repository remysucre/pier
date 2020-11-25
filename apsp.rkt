#lang rosette

(define BIG 99999999)

(define (I b)
  (if b 0 BIG))

(define (weight E x z ws)
  (s-min
   (lambda (w)
     (+ (I (E x z w)) w))
   ws))

(define (s-any f xs)
  (apply || (map f xs)))

(define (s-min f xs)
  (apply min (map f xs)))

(define (rule-R R E x z w ys w1s w2s)
  (|| (E x z w)
      (s-any
       (lambda (y)
         (s-any
          (lambda (w1)
            (s-any
             (lambda (w2)
               (&& (R x y w1)
                   (E y z w2)
                   (= w (+ w1 w2))))
             w2s))
          w1s))
       ys)))

(define (rule-S R E x z ws ys w1s w2s)
  (s-min
   (lambda (w)
     (+ (I (rule-R R E x z w ys w1s w2s))
        w))
   ws))

(define (S R x z ws)
  (s-min (lambda (w) (+ (I (R x z w)) w)) ws))

(define (rule-S-opt R E x z ws ys w1s w2s)
  (min (weight E x z ws)
       (s-min
        (lambda (y)
          (+ (S R x y w1s)
             (weight E y z w2s)))
        ys)))

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x z integer?)

(define ys (range 4))
(define ws (range 8))
(define w1s (range 4))
(define w2s (range 4))

(verify (assert (= (rule-S R E x z ws ys w1s w2s)
                   (rule-S-opt R E x z ws ys w1s w2s))))
