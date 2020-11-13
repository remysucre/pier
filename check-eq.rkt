#lang rosette

(require (for-syntax syntax/parse) racket/set)

(define (I b)
  (if b 1 0))

;; remove rel and var tags
(define-for-syntax (to-rkt e)
  (match e
    [`(rel ,x ...) x]
    [`(var ,v) v]
    [`(,op ,x ...) (cons op (map to-rkt x))]
    [n n]))

(define-for-syntax (fvs e)
  (match e
    [`(var ,v) (set v)]
    [`(sum (var ,i) ,e) (set-remove (fvs e) i)]
    [`(,op ,e ...) (apply set-union (map fvs e))]
    [else (list->set (set))]))

(define-syntax (interp stx)
  (syntax-parse stx
    [(_ e1 e2)
     (define vars (set->list (set-union (fvs (syntax->datum #`e1)) (fvs (syntax->datum #`e2)))))
     (define defs #`(define-symbolic #,@vars integer?))
     (define lhs (datum->syntax defs (to-rkt (syntax->datum #`e1))))
     (define rhs (datum->syntax defs (to-rkt (syntax->datum #`e2))))
     #`(begin
         #,defs
         (verify (assert (= #,lhs #,rhs))))]))

(interp (+ (var x) (var x)) (* 2 (var x)))
