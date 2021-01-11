#lang rosette

(define (process e)
  (match e
    [`(var ,x) x]
    [`(rel ,R ,vs ...) (apply (get-rel R) (map process vs))]
    [`(I ,e) (op-I-BT (process e))]
    [`(* ,x ,y) (op-t* (process x) (process y))]
    [`(+ ,x ,y) (op-t+ (process x) (process y))]
    ;; HACK hard coding sum types
    [`(sum w1 ,e) (op-sum-t-t 'w1 (process e))]
    [`(sum y ,e) (op-sum-i-t 'y (process e))]
    [`(,udf ,vs ...) (apply (get-udf udf) (map process vs))]))


(struct rel-R (x y z) #:transparent)
(struct op-I-BT (e) #:transparent)
(struct op-t* (x y) #:transparent)
(struct op-t+ (x y) #:transparent)
(struct op-sum-t-t (x y) #:transparent)
(struct op-sum-i-t (x y) #:transparent)

(struct op-weight (w x y) #:transparent)

(define (get-udf f)
  (match f
    ['weight op-weight]))

(define (get-rel R)
  (match R
    ['R rel-R]))

(process '(+
  (weight (var w) (var x) (var z))
  (sum y
    (sum w1
      (* (weight (var w2) (var y) (var z))
        (* (var w1)
          (I (rel R (var x) (var y) (var w1)))))))))
