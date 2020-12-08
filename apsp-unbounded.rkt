#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

;; TODO use union instead?
;; tropical number; infty if i
(struct trop (i n) #:transparent)

;; #t => n, #f => infty
(define (I b)
  (if b (trop #f 0) (trop #t 1)))

;; uninterpreted tropical summation
(define-symbolic* sig-ii-i boolean?)
(define-symbolic* sig-ii-n real?)
(define sig-ii (trop sig-ii-i sig-ii-n))

(define-symbolic* sig-in-i (~> real? boolean?))
(define-symbolic* sig-in-n (~> real? real?))
(define (sig-in n) (trop (sig-in-i n) (sig-in-n n)))

(define-symbolic* sig-ni-i (~> real? boolean?))
(define-symbolic* sig-ni-n (~> real? real?))
(define (sig-ni n) (trop (sig-ni-i n) (sig-ni-n n)))

(define-symbolic* sig-nn-i (~> real? real? boolean?))
(define-symbolic* sig-nn-n (~> real? real? real?))
(define (sig-nn x y) (trop (sig-nn-i x y) (sig-nn-n x y)))

(define (sig v e)
  (destruct v
    [(trop i n)
     (destruct e
       [(trop j m)
        (if i
            (if j
                sig-ii
                (sig-in m))
            (if j
                (sig-ni n)
                (sig-nn n m)))])]))

(define-symbolic* sig-int-i (~> integer? boolean? real? boolean?))
(define-symbolic* sig-int-n (~> integer? boolean? real? real?))

(define (sig-int v e)
  (destruct e
    [(trop i n)
     (trop (sig-int-i v i n)
           (sig-int-n v i n))]))

;; TODO check symbolic def
(define (weight x z)
  (define (w-i) (define-symbolic* b boolean?) b)
  (define (w-n) (define-symbolic* x real?) x)
  (define w (trop (w-i) (w-n)))
  (sig w (t* w (I (E x z w)))))

(define (t= a b)
  (destruct a
    [(trop i n)
     (destruct b
       [(trop j m)
        (if i j
            (if j i
                (eq? m n)))])]))

;; t+ is min
(define (t+ a b)
  (destruct a
    [(trop i n)
     (if i b
         (destruct b
           [(trop j m)
            (if j a
                (trop #f (min m n)))]))]))

;; t* is +
(define (t* a b)
  (destruct a
    [(trop i n)
     (if i a
         (destruct b
           [(trop j m)
            (if j b
                (trop #f (+ m n)))]))]))

;; TODO try real for x y
;; FIXME same problem as sig for (boolean? real?)
(define-symbolic E-i-n (~> integer? integer? boolean? real? boolean?))
(define (E x y w)
  (destruct w
    [(trop i n)
     (E-i-n x y i n)]))

(define-symbolic R-i-n (~> integer? integer? boolean? real? boolean?))
(define (R x y w)
  (destruct w
    [(trop i n)
     (R-i-n x y i n)]))

(define-symbolic y-i w1-i w2-i boolean?)
(define-symbolic y-n w1-n w2-n real?)
;; (define y (trop y-i y-n))
(define-symbolic y integer?)

(define w1 (trop w1-i w1-n))
(define w2 (trop w2-i w2-n))

(define (R-code x z w)
  (op-t+ (op-I (rel-E x z w))
      (op-sum-int 'y
           (op-sum 'w1
                (op-sum 'w2
                     (op-t* (op-I (rel-R x 'y 'w1))
                         (op-t* (op-I (rel-E 'y x 'w2))
                             (op-I (op-eq? w (op-t* 'w1 'w2))))))))))

(define-symbolic w-i boolean?)
(define-symbolic w-n real?)

(define w (trop w-i w-n))

;; (define (rule-S x z)
;;   (sig w (t* (I (rule-R x z w)) w)))

;; (define (S x z)
;;   (sig w (t* (I (R x z w)) w)))

;; (define (S-opt x z)
;;   (t+ (weight x z)
;;       (sig y (t* (S x y) (weight y z)))))

(struct op-weight (x y) #:transparent)
(struct op-sum (v e) #:transparent)
(struct op-sum-int (v e) #:transparent)
(struct op-t+ (x y) #:transparent)
(struct op-t* (x y) #:transparent)
(struct op-I (e) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct rel-E (x y w) #:transparent)
(struct rel-R (x y w) #:transparent)

(define (interpret p)
  (destruct p
    [(op-t* x y)
     (t* (interpret x )
         (interpret y))]
    [(op-t+ x y)
     (t+ (interpret x)
         (interpret y))]
    [(op-sum v e)
     (sig (interpret v)
          (interpret e))]
    [(op-sum-int v e)
     (sig-int (interpret v)
              (interpret e))]
    [(op-weight x y)
     (weight (interpret x)
             (interpret y))]
    [(trop i n) p]
    [(op-I e)
     (I (interpret e))]
    [(rel-E x y w)
     (E (interpret x) (interpret y) (interpret w))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [_ (cdr (assoc p vars))]))

(define-symbolic x z integer?)

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'w w)
        (cons 'w1 w1)
        (cons 'w2 w2)))

(define (var? x)
  (member x '(x y z w w1 w2)))

(define (fvs p)
  (destruct p
    [(op-t* x y) (append (fvs x) (fvs y))]
    [(op-t+ x y) (append (fvs x) (fvs y))]
    [(op-eq? x y) (append (fvs x) (fvs y))]
    [(op-sum v e) (remove* (fvs v) (fvs e))]
    [(op-sum-int v e) (remove* (fvs v) (fvs e))]
    [(op-weight x y) (list x y)]
    [(op-I e) (fvs e)]
    [(rel-E x y w) (append (fvs x) (fvs y) (fvs w))]
    [(rel-R x y w) (append (fvs x) (fvs y) (fvs w))]
    [_ (if (var? p) (list p) '())]))

(assert (eq? (fvs 'x) '(x)))
(assert (eq? (fvs (rel-E 'x 'y 'w)) '(x y w)))
(assert (eq? (fvs (op-t* (rel-E 'x 'y 'w) (rel-E 'x 'y 'w)))
             '(x y w x y w)))
(assert (eq? (fvs (op-sum 'w (rel-E 'x 'y 'w))) '(x y)))
(assert (eq? (fvs (R-code 'x 'z 'w)) '(x z w x x w)))

(define (factors e)
  (destruct e
    [(op-t* x y) (append (factors x) (factors y))]
    [_ (list e)]))

(define (prod xs)
  (cond
    [(empty? xs) (trop #f 0)]
    [else (foldr (lambda (x p) (op-t* x p)) (car xs) (cdr xs))]))

;; push v into e
(define (push-var v e)
  (define-values (l r)
    (partition (lambda (f) (member v (fvs f))) (factors e)))
  (cons (prod l) (prod r)))

;; assumes normalized p
;; i.e. adding sums of products
(define (push-sum p)
  (destruct p
    [(op-t+ x y) (op-t+ (push-sum x) (push-sum y))]
    [(op-t* x y) (op-t* (push-sum x) (push-sum y))]
    [(op-sum v e)
     (define lr (push-var v (push-sum e)))
     (op-t* (op-sum v (car lr)) (cdr lr))]
    [(op-sum-int v e)
     (define lr (push-var v (push-sum e)))
     (op-t* (op-sum-int v (car lr)) (cdr lr))]
    [_ p]))

;; (op-sum 'w (op-t* (op-I (op-eq? 'w x)) 'w))
;; x
(define (simplify p)
  (destruct p
    [(op-sum v e)
     (if (eq? p (op-sum 'w (op-t* (op-I (op-eq? 'w (op-t* 'w1 'w2))) 'w)))
         (op-t* 'w1 'w2)
         (op-sum v (simplify e)))]
    [(op-sum-int v e) (op-sum-int v (simplify e))]
    [(op-t+ x y) (op-t+ (simplify x) (simplify y))]
    [(op-t* x y) (op-t* (simplify x) (simplify y))]
    [_ p]))

;; MIN_{y,w1,w2,w} w * R(x,y,w1) * E(y,x,w2) * 1_{w=w1*w2}
(define l
  (interpret
   (push-sum
    (simplify
     (push-sum
      (op-sum-int
       'y
       (op-sum
        'w1
        (op-sum
         'w2
         (op-sum
          'w
          (op-t* 'w
                 (op-t* (op-I (rel-R 'x 'y 'w1))
                        (op-t* (op-I (rel-E 'y 'x 'w2))
                               (op-I (op-eq? 'w (op-t* 'w1 'w2)))))))))))))))

;; MIN_{y,w1,w2} R(x,y,w1) * E(y,x,w2) * w1 * w2
(define r
  (interpret
   (simplify
    (push-sum
     (op-sum-int
      'y
      (op-sum
       'w1
       (op-sum
        'w2
        (op-t* (op-I (rel-R 'x 'y 'w1))
               (op-t* (op-I (rel-E 'y 'x 'w2))
                      (op-t* 'w1 'w2))))))))))

(verify (assert (t= l r)))
