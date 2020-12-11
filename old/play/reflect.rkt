#lang rosette

(require (for-syntax syntax/parse) racket/set)

(define env (make-hash))

(define-syntax (var stx)
  (syntax-parse stx
    [(_ v) #'(hash-ref! env 'v (lambda () (define-symbolic* v integer?) v))]))

(define-syntax (check-eq stx)
  (syntax-parse stx
    [(_ e1 e2) #'(verify (assert (= e1 e2)))]))

(check-eq (+ (var x) (var x)) (* 2 (var x)))
(check-eq (+ (var x) (var y)) (+ (var y) (var x)))
(check-eq (* (var z) (+ (var x) (var y)))
          (+ (* (var z) (var y)) (* (var z) (var x))))

(check-eq (+ (var x) (var x) (var x)) (* 2 (var x)))
(check-eq (+ (var x) (var y)) (+ (var y) (var y)))
