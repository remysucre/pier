#lang rosette

(define BIG 99999999)

(define (I b)
  (if b 0 BIG))

(define (weight E x z ws-1)
  (s-min
   (lambda (w)
     (+ (I (E x z w)) w))
   ws-1))

(define (s-any f xs)
  (apply || (map f xs)))

(define (s-min f xs)
  (apply min (map f xs)))

(define (rule-R R E x z w ws-2)
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
             ws-2))
          ws-2))
       ws-2)))

(define (rule-S R E x z ws-1 ws-2)
  (s-min
   (lambda (w)
     (+ (I (rule-R R E x z w ws-2))
        w))
   ws-1))

(define (S R x z ws-1)
  (s-min (lambda (w) (+ (I (R x z w)) w)) ws-1))

(define (rule-S-opt R E x z ws-1 ws-2)
  (min (weight E x z ws-1)
       (s-min
        (lambda (y)
          (+ (S R x y ws-2)
             (weight E y z ws-2)))
        ws-2)))

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x z integer?)

(define ws-1 (range 8))
(define ws-2 (range 4))

(verify (assert (= (rule-S R E x z ws-1 ws-2)
                   (rule-S-opt R E x z ws-1 ws-2))))
