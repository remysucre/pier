#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt")

(provide (all-defined-out))

;; symbolics

(define-symbolic E-i-n (~> integer? integer? boolean?))
(define-symbolic E-n-n (~> integer? integer? real? boolean?))
(define (E x y w) (if (boolean? w) (E-i-n x y) (E-n-n x y w)))

;; NOTE check symbolic def
(define (weight x z)
  (define (w-b) (define-symbolic* b boolean?) b)
  (define (w-n) (define-symbolic* x real?) x)
  (define w (if (w-b) #t (w-n)))
  (sig w (t* w (I (E x z w)))))

(define-symbolic R-i-n (~> integer? integer? boolean?))
(define-symbolic R-n-n (~> integer? integer? real? boolean?))
(define (R x y w) (if (boolean? w) (R-i-n x y) (R-n-n x y w)))

(define-symbolic x y z integer?)
(define-symbolic w-i w1-i w2-i boolean?)
(define-symbolic w-n w1-n w2-n real?)
(define w (if w-i #t w-n))
(define w1 (if w1-i #t w1-n))
(define w2 (if w2-i #t w2-n))

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'w w)
        (cons 'w1 w1)
        (cons 'w2 w2)))

(define (var? x)
  (member x '(x y z w w1 w2)))

(define (R-code x z w)
  (op-t+ (op-I (rel-E x z w))
      (op-sum-int 'y
           (op-sum 'w1
                (op-sum 'w2
                     (op-t* (op-I (rel-R x 'y 'w1))
                         (op-t* (op-I (rel-E 'y x 'w2))
                             (op-I (op-eq? w (op-t* 'w1 'w2))))))))))