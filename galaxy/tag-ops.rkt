#lang rosette

(define (process e)
  (match e
    [`(var ,x) x]
    [`(rel ,R ,vs ...) (apply (get-rel R) (map process vs))]
    [`(I ,e) (op-I-BN (process e))]
    [`(* ,x ,y) (op-* (process x) (process y))]
    [`(+ ,x ,y) (op-+ (process x) (process y))]
    [`(= ,x ,y) (op-eq? (process x) (process y))]
    ;; HACK hard coding sum types
    [`(sum ,y ,e) (op-sum-i-i y (process e))]
    [`(,udf ,vs ...) (apply (get-udf udf) (map process vs))]
    [n n]))


(struct rel-G (x z) #:transparent)
(struct rel-P (p x t) #:transparent)
(struct op-I-BN (e) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-+ (x y) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct op-sum-i-i (x y) #:transparent)

(struct op-- (x y) #:transparent)
(struct op-leq (x y) #:transparent)
(struct op-gt (x y) #:transparent)
(struct op-lt (x y) #:transparent)

(define (get-udf f)
  (match f
    ['- op--]
    ['<= op-leq]
    ['< op-lt]
    ['> op-gt]
    ['= op-eq?]))

(define (get-rel R)
  (match R
    ['G rel-G]
    ['P rel-P]))

(process '(* (rel G (var t) (var x))
             (I (<= 5
                    (sum p
                         (* (rel P (var p) (var x) (var t))
                            (rel P (var p) (var y) (+ 1 (var t))))))))
 #;(-
  (+
    (sum
      w
      (sum
        j
        (*
          (*
            (var w)
            (*
              (I (<= 1 (var j)))
              (I (<= (var j) (var t)))))
          (*
            (I (= (var j) (var t)))
            (I (rel v (var j) (var w)))))))
    (sum
      w
      (sum
        j
        (*
          (*
            (var w)
            (*
              (I (<= 1 (var j)))
              (I (<= (var j) (var t)))))
          (*
            (I (rel R (- (var t) 1) (var j) (var w)))
            (*
              (I (< (var j) (var t)))
              (I (> (var t) 1))))))))
  (+
    (sum
      w
      (sum
        j
        (*
          (*
            (var w)
            (*
              (I (<= 1 (var j)))
              (I (<= (var j) (- (var t) (var k))))))
          (*
            (I (rel v (var j) (var w)))
            (I (= (var j) (- (var t) (var k))))))))
    (sum
      w
      (sum
        j
        (*
          (*
            (var w)
            (*
              (I (<= 1 (var j)))
              (I (<= (var j) (- (var t) (var k))))))
          (*
            (I (rel
              R
              (- (- (var t) (var k)) 1)
              (var j)
              (var w)))
            (*
              (I (< (var j) (- (var t) (var k))))
              (I (> (- (var t) (var k)) 1))))))))))
