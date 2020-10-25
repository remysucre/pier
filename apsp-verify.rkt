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
