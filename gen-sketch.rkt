#lang rosette

(require rosette/lib/destruct)
(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`
(require "ops.rkt"
         "apsp/interpret.rkt")

;; HACK hard-coding vars and weights
(define (var? p)
  (member p (list 'x 'y 'z)))

(define (weight? p)
  (member p (list 'w 'w1 'w2)))

;; Generate vars and weights

(define (gen-v-dup p)
  (match p
    [`(,op ,args ...) (apply append (map gen-v args))]
    [(? var?) (list p)]
    [_ (list)]))

(define (gen-v p)
  (remove-duplicates (gen-v-dup p)))

(define (gen-w-dup p)
  (match p
    [`(,op ,args ...) (apply append (map gen-w args))]
    [(? weight?) (list p)]
    [_ (list)]))

(define (gen-w p)
  (remove-duplicates (gen-w-dup p)))

;; Generate terminals (relations and UDFs)
;; TODO should take in a list of decls

(define (gen-rel ds p)
  (define (??v) (apply choose* (gen-v p)))
  (define (??w) (apply choose* (gen-w p)))
  (list (op-I-BT (rel-E (??v) (??v) (??w)))
        (op-I-BT (rel-R (??v) (??v) (??w)))))

(define (gen-rel-udf ds p)
  (define (??v) (apply choose* (gen-v p)))
  (define (??w) (apply choose* (gen-w p)))
  (list (op-weight (??w) (??v) (??v))))

(define (gen-terminal ds p)
  (define (??w) (apply choose* (gen-w p)))
  (apply choose* (cons (??w)
         (append (gen-rel ds p)
                 (gen-rel-udf ds p)))))

;; Generate non-terminals (operators, sums)
;; TODO find what sums are used

(define (gen-nonterm ds p)
  (define (??op) (choose* op-t+ op-t*))
  (define (??v) (apply choose* (gen-v p)))
  (define (??w) (apply choose* (gen-w p)))
  (define (??term depth)
    (if (= depth 0)
        (gen-terminal ds p)
        (choose* ((??op) (??term (- depth 1)) (??term (- depth 1)))
             (op-sum-t-t (??w) (??term (- depth 1)))
             (op-sum-i-t (??v) (??term (- depth 1))))))
  ??term)

;; Additional layers of sums
;; TODO find what sums are used

(define (gen-agg p)
  (define (??v) (apply choose* (gen-v p)))
  (define (??w) (apply choose* (gen-w p)))
  (define (??agg depth e)
  (if (= depth 0)
      e
      (choose* (op-sum-i-t (??v) (??agg (- depth 1) e))
               (op-sum-t-t (??w) (??agg (- depth 1) e)))))
  ??agg)

;; TODO Generate sketch from normalized input

(define (gen-sketch ds p)
  (define ??term (gen-nonterm ds p))
  (define ??agg (gen-agg p))
  (op-t+ (??term 0)
        (??agg 1
                (op-sum-t-t 'w1
                        (??agg 0
                        (op-t* (op-t* (op-I-BT (rel-R 'x 'y 'w1)) 'w1)
                               (??term 0)))))))

(define input '(+
  (weight (var w) (var x) (var z))
  (sum y
    (sum w1
      (* (weight (var w2) (var y) (var z))
        (* (var w1)
          (I (rel R (var x) (var y) (var w1)))))))))

(define prog
(op-t+
 (op-weight 'w 'x 'z)
 (op-sum-i-t 'y (op-sum-t-t 'w1 (op-t* (op-weight 'w2 'y 'z) (op-t* 'w1 (op-I-BT (rel-R 'x 'y 'w1))))))))

(define sketch (gen-sketch 0 input))

(define M
  (synthesize
   #:forall (list R-i-n R-n-n E-i-n E-n-n
                  x y z
                  w-i w1-i w2-i w-n w1-n w2-n
                  sum-t-inf-inf-r sum-t-inf-inf-b sum-t-inf-r-r sum-t-inf-r-b sum-t-r-inf-r sum-t-r-inf-b sum-t-r-r-r sum-t-r-r-b
                  sum-t-i-inf-b sum-t-i-inf-i sum-t-i-i-b sum-t-i-i-i)
   #:guarantee (assert (eq? (interpret sketch) (interpret prog)))))

(evaluate sketch M)
