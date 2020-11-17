#lang rosette

(define (to-num b)
  (if b 1 0))

(define (s-sum f)
  (lambda (w) (apply + (map f w))))

(define (rule-R v R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (< j t)
          (> t 1))))

(define (rule-S v R t js ws)
  ((s-sum
    (lambda (j)
      ((s-sum
        (lambda (w)
          (* w
             (to-num
              (&& (rule-R v R t j w)
                  (<= 1 j)
                  (<= j t))))))
       ws)))
   js))

(define (S v R t js ws)
  ((s-sum
    (lambda (j)
      ((s-sum
        (lambda (w)
          (* w
             (to-num
              (&& (R t j w)
                  (<= 1 j)
                  (<= j t))))))
       ws)))
   js))

(define (rule-S-opt v R t js ws)
  (+ (S v R (- t 1) js ws)
     ((s-sum
       (lambda (j)
         ((s-sum
           (lambda (w)
             (* w
                (to-num
                 (&& (v j w)
                     (= t j)
                     (<= 1 j))))))
          ws)))
      js)))

(define (rule-S-out v R t js ws)
  (+ (S v R (- t 1) js ws)
     (vec-get v t)))

(define (vec-get v t)
  ((s-sum
    (lambda (w)
      (* w
         (to-num
          (&& (v t w)
              (<= 1 t))))))
   ws))

(define-symbolic t tt j w integer?)
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))

(define N 4)
(define js (range N))
(define ws (range N))

(assert (<= 0 t))
(assert (< t N))

(verify (assert (= (rule-S v R t js ws) (rule-S-out v R t js ws))))

(require rosette/lib/synthax)

(define-synthax (semiring v R t js ws depth)
  #:base (choose (S v R (- t 1) js ws) v R js ws t)
  #:else (choose (S v R (- t 1) js ws) v R js ws t
                 (+ (semiring v R t js ws (- depth 1))
                    (semiring v R t js ws (- depth 1)))
                 (vec-get (semiring v R t js ws (- depth 1))
                          (semiring v R t js ws (- depth 1)))))

(define (optimized v R t js ws)
  (semiring v R t js ws 3))

(define OPT
  (synthesize
   #:forall (list v R t)
   #:guarantee (assert (= (optimized v R t js ws) (rule-S v R t js ws)))))

(print-forms OPT)

;; (define (rule-S-58 v R t js ws)
;;   ((s-sum
;;     (lambda (j)
;;       ((s-sum
;;         (lambda (w)
;;           (* w
;;              (to-num
;;               (&& (|| (&& (v j w) (= t j))
;;                       (&& (R (- t 1) j w)
;;                           (< j t)
;;                           (> t 1)))
;;                   (<= 1 j)
;;                   (<= j t))))))
;;        ws)))
;;    js))

;; (define (rule-S-59 v R t js ws)
;;   (+ ((s-sum
;;        (lambda (j)
;;          ((s-sum
;;            (lambda (w)
;;              (* w
;;                 (to-num
;;                  (&& (v j w)
;;                      (= t j)
;;                      (<= 1 j)
;;                      (<= j t))))))
;;           ws)))
;;       js)
;;      ((s-sum
;;        (lambda (j)
;;          ((s-sum
;;            (lambda (w)
;;              (* w
;;                 (to-num
;;                  (&& (R (- t 1) j w)
;;                      (< j t)
;;                      (> t 1)
;;                      (<= 1 j)
;;                      (<= j t))))))
;;           ws)))
;;       js)))

;; (define (rule-S-60 v R t js ws)
;;   (+ ((s-sum
;;        (lambda (j)
;;          ((s-sum
;;            (lambda (w)
;;              (* w
;;                 (to-num
;;                  (&& (v j w)
;;                      (= t j)
;;                      (<= 1 j))))))
;;           ws)))
;;       js)
;;      ((s-sum
;;        (lambda (j)
;;          ((s-sum
;;            (lambda (w)
;;              (* w
;;                 (to-num
;;                  (&& (R (- t 1) j w)
;;                      (< j t)
;;                      (> t 1)
;;                      (<= 1 j))))))
;;           ws)))
;;       js)))

;; (define (rule-S-61 v R t js ws)
;;   (+ ((s-sum
;;        (lambda (j)
;;          ((s-sum
;;            (lambda (w)
;;              (* w
;;                 (to-num
;;                  (&& (v j w)
;;                      (= t j)
;;                      (<= 1 j))))))
;;           ws)))
;;       js)
;;      (* (to-num (> t 1))
;;         ((s-sum
;;           (lambda (j)
;;             ((s-sum
;;               (lambda (w)
;;                 (* w
;;                    (to-num
;;                     (&& (R (- t 1) j w)
;;                         (<= j (- t 1))
;;                         (<= 1 j))))))
;;              ws)))
;;          js))
;;      ))
