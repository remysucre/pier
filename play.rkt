#lang rosette/safe

(require rosette/lib/destruct)

(struct fn (var e) #:transparent)
(struct plus (x y) #:transparent)

(define (interpret p env)
  (destruct p
    [(fn var e)
     (lambda (x) (interpret e (cons (cons var x) env)))]
    [(plus x y) (+ (interpret x env) (interpret y env))]
    [_ (define result (assoc p env))
       (cond
         [result (cdr result)]
         [else (assert #f "free-var")])]))

((interpret (fn 'x (plus 'x 'x)) '()) 4)

(define-symbolic x integer?)

(verify (assert (= ((interpret (fn 'x (plus 'x 'x)) '()) x)
                   ((interpret (fn 'y (plus 'y 'y)) '()) x))))
