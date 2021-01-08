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


(struct rel-D (x z) #:transparent)
(struct rel-E (x z) #:transparent)
(struct rel-sigma (y z) #:transparent)
(struct op-I-BN (e) #:transparent)
(struct op-* (x y) #:transparent)
(struct op-+ (x y) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct op-sum-i-i (x y) #:transparent)

(struct op-inv (x) #:transparent)

(define (get-udf f)
  (match f
    ['inv op-inv]))

(define (get-rel R)
  (match R
    ['E rel-E]
    ['D rel-D]
    ['sigma rel-sigma]))

(process '(+
  (sum
    t
    (*
      (I
        (=
          (rel D (var s) (var t))
          (+
            (rel D (var s) (var v))
            (rel D (var v) (var t)))))
      (*
        (inv (rel sigma (var s) (var t)))
        (*
          (rel sigma (var s) (var v))
          (I (rel E (var v) (var t)))))))
  (sum
    t
    (sum
      u
      (*
        (I
          (=
            (rel D (var s) (var t))
            (+
              (rel D (var s) (var v))
              (rel D (var v) (var t)))))
        (*
          (inv (rel sigma (var s) (var t)))
          (*
            (rel sigma (var s) (var v))
            (*
              (*
                (rel sigma (var u) (var t))
                (I (rel E (var v) (var u))))
              (I
                (=
                  (rel D (var v) (var t))
                  (+ 1 (rel D (var u) (var t)))))))))))))
