#lang rosette

(require (for-syntax syntax/parse) racket/set)

(define (I b)
  (if b 1 0))

;; remove rel and var tags
(define-for-syntax (to-rkt e)
  (match e
    [`(rel ,x ...) (cons (car x) (map to-rkt (cdr x)))]
    [`(var ,v) v]
    [`(,op ,x ...) (cons op (map to-rkt x))]
    [n n]))

(define-for-syntax (fvs e)
  (match e
    [`(var ,v) (set v)]
    [`(sum (var ,i) ,e) (set-remove (fvs e) i)]
    [`(,op ,e ...) (apply set-union (map fvs e))]
    [else (set)]))

(define-for-syntax (rels e)
  (match e
    [`(rel ,r ,v ...) (set (cons r (length v)))]
    [`(,op ,e ...) (apply set-union (map rels e))]
    [else (set)]))

(define-for-syntax (defrels rs)
  (for/list ([rel rs])
    #`(define-symbolic #,(car rel) (~> #,@(make-list (cdr rel) #`integer?) integer?))))

(define-syntax (equivalent? stx)
  (syntax-parse stx
    [(_ e1 e2)
     (define vars (set->list (set-union (fvs (syntax->datum #`e1)) (fvs (syntax->datum #`e2)))))
     (define rs (set->list (set-union (rels (syntax->datum #`e1)) (rels (syntax->datum #`e2)))))
     (define defs #`(define-symbolic #,@vars integer?))
     (define rdefs (defrels rs))
     (define lhs (datum->syntax defs (to-rkt (syntax->datum #`e1))))
     (define rhs (datum->syntax defs (to-rkt (syntax->datum #`e2))))
     #`(begin
         #,defs
         #,@rdefs
         (verify (assert (= #,lhs #,rhs))))]))

#;(equivalent? (* (* (rel R (- (- (var t) (var k)) 1) (var j) (var w))
                         (I (> (var k) 0))
                         (* (I (> (- (var t) (var k)) 1))
                            (I (< (var j) (- (var t) (var k))))))
                (* (var w) (* (I (>= (var j) 1)) (I (<= (var j) (- (var t) (var k)))))))
             (* (* (rel R (- (- (var t) (var k)) 1) (var j) (var w)) (var w))
                      (I (> (var k) 0))
                      (* (* (I (<= (var j) (- (- (var t) (var k)) 1)))
                            (I (<= 1 (var j))))
                         (I (> (var t) 1)))))

(equivalent? (* (* (rel v (var j) (var w)) (I (= (var t) (var j))))
                      (* (var w) (* (I (>= (var j) 1))
                              (I (<= (var j) (var t))))))
             (* (* (rel v (var j) (var w)) (I (= (var t) (var j))))
                      (* (var w) (I (>= (var j) 1)))))
