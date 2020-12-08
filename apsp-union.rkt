#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

;; #t => 0, #f => infty
(define (I b) (if b 0 #t))

;; uninterpreted tropical summation

(define-symbolic sig-ii-n real?)
(define-symbolic sig-ii-b boolean?)
(define sig-ii (if sig-ii-b #t sig-ii-n))

(define-symbolic sig-in-n (~> real? real?))
(define-symbolic sig-in-b (~> real? boolean?))
(define (sig-in x)
  (if (sig-in-b x) #t (sig-in-n x)))

(define-symbolic sig-ni-n (~> real? real?))
(define-symbolic sig-ni-b (~> real? boolean?))
(define (sig-ni x)
  (if (sig-ni-b x) #t (sig-ni-n x)))

(define-symbolic sig-nn-n (~> real? real? real?))
(define-symbolic sig-nn-b (~> real? real? boolean?))
(define (sig-nn x y)
  (if (sig-nn-b x y) #t (sig-nn-n x)))

(define (sig v e)
  (if (boolean? v)
      (if (boolean? e)
          sig-ii
          (sig-in e))
      (if (boolean? e)
          (sig-ni v)
          (sig-nn v e))))

;; summation over integer domain

(define-symbolic sig-int-i-b (~> integer? boolean?))
(define-symbolic sig-int-i-n (~> integer? real?))
(define (sig-int-i v)
  (if (sig-int-i-b v) #t (sig-int-i-n v)))

(define-symbolic sig-int-n-b (~> integer? real? boolean?))
(define-symbolic sig-int-n-n (~> integer? real? real?))
(define (sig-int-n v e)
  (if (sig-int-n-b v e) #t (sig-int-n-n v e)))

(define (sig-int v e)
  (if (boolean? e)
      (sig-int-i v)
      (sig-int-n v e)))

(define (t+ a b)
  (if (boolean? a)
      b
      (if (boolean? b)
          a
          (min a b))))

(define (t* a b)
  (if (boolean? a)
      #t
      (if (boolean? b)
          #t
          (+ a b))))

(define-symbolic E-i-n (~> integer? integer? boolean?))
(define-symbolic E-n-n (~> integer? integer? real? boolean?))
(define (E x y w) (if (boolean? w) (E-i-n x y) (E-n-n x y w)))

(define-symbolic R-i-n (~> integer? integer? boolean?))
(define-symbolic R-n-n (~> integer? integer? real? boolean?))
(define (R x y w) (if (boolean? w) (R-i-n x y) (R-n-n x y w)))

;; TODO check symbolic def
(define (weight x z)
  (define (w-b) (define-symbolic* b boolean?) b)
  (define (w-n) (define-symbolic* x real?) x)
  (define w (if (w-b) #t (w-n)))
  (sig w (t* w (I (E x z w)))))

(struct op-weight (x y) #:transparent)
(struct op-sum (v e) #:transparent)
(struct op-sum-int (v e) #:transparent)
(struct op-t+ (x y) #:transparent)
(struct op-t* (x y) #:transparent)
(struct op-I (e) #:transparent)
(struct op-eq? (x y) #:transparent)
(struct rel-E (x y w) #:transparent)
(struct rel-R (x y w) #:transparent)

(define (R-code x z w)
  (op-t+ (op-I (rel-E x z w))
      (op-sum-int 'y
           (op-sum 'w1
                (op-sum 'w2
                     (op-t* (op-I (rel-R x 'y 'w1))
                         (op-t* (op-I (rel-E 'y x 'w2))
                             (op-I (op-eq? w (op-t* 'w1 'w2))))))))))

(define-symbolic x y z integer?)
(define-symbolic w-i w1-i w2-i boolean?)
(define-symbolic w-n w1-n w2-n real?)
(define w (if w-i #t w-n))
(define w1 (if w1-i #t w1-n))
(define w2 (if w2-i #t w2-n))

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'w w)
        (cons 'w1 w1)
        (cons 'w2 w2)))

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
    [(op-I e)
     (I (interpret e))]
    [(rel-E x y w)
     (E (interpret x) (interpret y) (interpret w))]
    [(rel-R x y w)
     (R (interpret x) (interpret y) (interpret w))]
    [_ (define result (assoc p vars))
       (if result (cdr result) p)]))

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
    [(empty? xs) 0]
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

(verify (assert (eq? l r)))
