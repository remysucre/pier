#lang rosette

(require (for-syntax syntax/parse) racket/set)

;; remove var tags
(define-for-syntax (to-rkt e)
  (match e
    [`(var ,v) v]
    [`(,op ,x ...) (cons op (map to-rkt x))]
    [n n]))

(define-for-syntax (fvs e)
  (match e
    [`(var ,v) (set v)]
    [`(,op ,e ...) (apply set-union (map fvs e))]
    [else (list->set (set))]))

(define-syntax (check-eq stx)
  (syntax-parse stx
    [(_ e1 e2)
     (define vars (set->list (set-union (fvs (syntax->datum #`e1)) (fvs (syntax->datum #`e2)))))
     (define defs #`(define-symbolic #,@vars integer?))
     (define lhs (datum->syntax defs (to-rkt (syntax->datum #`e1))))
     (define rhs (datum->syntax defs (to-rkt (syntax->datum #`e2))))
     #`(begin
         #,defs
         (verify (assert (= #,lhs #,rhs))))]))

(check-eq (+ (var x) (var x)) (* 2 (var x)))
