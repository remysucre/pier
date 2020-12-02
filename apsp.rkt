#lang rosette

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(define BIG 99999999)

(define (I b)
  (if b 0 BIG))

(define (weight E x z ws-1)
  (s-min
   (lambda (w)
     (+ (I (E x z w)) w))
   ws-1))

(define (s-any f xs)
  (apply || (map f xs)))

(define (s-min f xs)
  (apply min (map f xs)))

(define (rule-R R E x z w ws-2)
  (|| (E x z w)
      (s-any
       (lambda (y)
         (s-any
          (lambda (w1)
            (s-any
             (lambda (w2)
               (&& (R x y w1)
                   (E y z w2)
                   (= w (+ w1 w2))))
             ws-2))
          ws-2))
       ws-2)))

(define (rule-S R E x z ws-1 ws-2)
  (s-min
   (lambda (w)
     (+ (I (rule-R R E x z w ws-2))
        w))
   ws-1))

(define (S R x z ws-1)
  (s-min (lambda (w) (+ (I (R x z w)) w)) ws-1))

(define (rule-S-opt R E x z ws-1 ws-2)
  (min (weight E x z ws-1)
       (s-min
        (lambda (y)
          (+ (S R x y ws-2)
             (weight E y z ws-2)))
        ws-2)))

(define (??var) (choose* x z))

(define (??ws) (choose* ws-1 ws-2))

(define (??op) (choose* + min))

;; (define (??term depth)
;;   (if (= depth 1)
;;       (choose* (S R (??var) (??var) (??ws))
;;                (weight E (??var) (??var) (??ws)))
;;       (choose* ((??op) (??term (- depth 1))
;;                        (??term (- depth 1)))
;;                (s-min (lambda ((??var)) (??term (- depth 1)))))))

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x y z integer?)

(define ws-1 (range 8))
(define ws-2 (range 4))

(define fvs
  (list (cons 'R R)
        (cons 'E E)
        (cons 'x x)
        (cons 'y y)
        (cons 'z z)
        (cons 'ws-1 ws-1)
        (cons 'ws-2 ws-2)))

(struct op-weight (E x y ws) #:transparent)
(struct rel-S (R x y ws) #:transparent)
(struct op-smin (f ws) #:transparent)
(struct op-min (x y) #:transparent)
(struct op-plus (x y) #:transparent)
(struct fn (var e) #:transparent)

(define (interpret p env)
  (destruct p
    [(fn var e)
     (lambda (x)
       (interpret e (cons (cons var x) env)))]
    [(op-plus x y)
     (+ (interpret x env)
        (interpret y env))]
    [(op-min x y)
     (min (interpret x env)
          (interpret y env))]
    [(op-smin f ws)
     (s-min (interpret f env)
            (interpret ws env))]
    [(rel-S R x y ws)
     (S (interpret R env)
        (interpret x env)
        (interpret y env)
        (interpret ws env))]
    [(op-weight E x y ws)
     (weight (interpret E env)
             (interpret x env)
             (interpret y env)
             (interpret ws env))]
    [_ (define result (assoc p env))
       (cond
         [result (cdr result)]
         [else (cdr (assoc p fvs))])]))

(define sketch
  (op-min (op-weight 'E 'x 'z 'ws-1)
          (op-smin
           (fn 'y
             (op-plus (rel-S 'R 'x 'y 'ws-2)
                      (op-weight 'E 'y 'z 'ws-2)))
           'ws-2)))

(verify (assert (= (interpret sketch '()) (rule-S R E x z ws-1 ws-2))))
