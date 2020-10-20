#lang rosette

(define BIG 99999999)

(define (to-int b)
  (if b 0 BIG))

(define (s-any f xs)
  (apply || (map f xs)))

(define (s-min f xs)
  (apply min (map f xs)))

(define (rule-R R E x z w ys w1s w2s)
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
             w2s))
          w1s))
       ys)))

(define (rule-S R E x z ws ys w1s w2s)
  (s-min
   (lambda (w)
     (+ (to-int (rule-R R E x z w ys w1s w2s))
        w))
   ws))

(define (S R x z ws)
  (s-min (lambda (w) (+ (to-int (R x z w)) w)) ws))

(define (rule-S-opt R E x z ws ys w1s w2s)
  (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
       (s-min
        (lambda (y)
          (+ (S R x y w1s)
             (s-min
              (lambda (w2)
                (+ (to-int (E y z w2)) w2)) w2s)))
        ys)))

(define-symbolic R E (~> integer? integer? integer? boolean?))
(define-symbolic x z integer?)
(define ys (range 4))
(define ws (range 8))
(define w1s (range 4))
(define w2s (range 4))

(verify (assert (! (= (rule-S R E x z ws ys w1s w2s)
                      (rule-S-opt R E x z ws ys w1s w2s)))))
(verify (assert (= (rule-S R E x z ws ys w1s w2s)
                   (rule-S-opt R E x z ws ys w1s w2s))))

;; (define (S-27 R E x z ws ys w1s w2s)
;;   (s-min
;;    (lambda (w)
;;      (+ (to-int
;;          (|| (E x z w)
;;              (s-any
;;               (lambda (y)
;;                 (s-any
;;                  (lambda (w1)
;;                    (s-any
;;                     (lambda (w2)
;;                       (&& (R x y w1)
;;                           (E y z w2)
;;                           (= w (+ w1 w2))))
;;                     w2s))
;;                  w1s))
;;               ys)))
;;         w))
;;    ws))

;; (define (S-28 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (w)
;;           (+ (to-int
;;               (s-any
;;                (lambda (y)
;;                  (s-any
;;                   (lambda (w1)
;;                     (s-any
;;                      (lambda (w2)
;;                        (&& (R x y w1)
;;                            (E y z w2)
;;                            (= w (+ w1 w2))))
;;                      w2s))
;;                   w1s))
;;                ys))
;;              w))
;;         ws)))

;; (define (S-29 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (w)
;;           (s-min
;;            (lambda (y)
;;              (s-min
;;               (lambda (w1)
;;                 (s-min
;;                  (lambda (w2)
;;                    (+ w
;;                       (to-int
;;                        (&& (R x y w1)
;;                            (E y z w2)
;;                            (= w (+ w1 w2))))))
;;                  w2s))
;;               w1s))
;;            ys))
;;         ws)))

;; (define (S-29 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (y)
;;           (s-min
;;            (lambda (w1)
;;              (s-min
;;               (lambda (w2)
;;                 (+ (s-min
;;                     (lambda (w)
;;                       (+ (to-int (= w (+ w1 w2)))
;;                          w))
;;                     ws)
;;                    (to-int
;;                     (&& (R x y w1)
;;                         (E y z w2)))))
;;               w2s))
;;            w1s))
;;         ys)))

;; (define (S-30 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (y)
;;           (s-min
;;            (lambda (w1)
;;              (s-min
;;               (lambda (w2)
;;                 (+ w1 w2
;;                    (to-int
;;                     (&& (R x y w1)
;;                         (E y z w2)))))
;;               w2s))
;;            w1s))
;;         ys)))

;; (define (S-31 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (y)
;;           (s-min
;;            (lambda (w1)
;;              (s-min
;;               (lambda (w2)
;;                 (+ (to-int (R x y w1))
;;                    (to-int (E y z w2))
;;                    w1 w2))
;;               w2s))
;;            w1s))
;;         ys)))

;; (define (S-32 R E x z ws ys w1s w2s)
;;   (min (s-min (lambda (w) (+ w (to-int (E x z w)))) ws)
;;        (s-min
;;         (lambda (y)
;;           (+ (s-min
;;               (lambda (w1)
;;                 (+ (to-int (R x y w1)) w1)) w1s)
;;              (s-min
;;               (lambda (w2)
;;                 (+ (to-int (E y z w2)) w2)) w2s)))
;;         ys)))
