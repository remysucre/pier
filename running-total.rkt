#lang rosette

(define N 4)

;; b is an upperbound of f(w)

(define (ub w b f geq)
  (forall (list w) (geq b (f w))))

;; lb is a lub of f

(define (lub b w lb f geq)
  (&& (ub w lb f geq)
      (forall (list b)
          (=> (ub w b f geq)
              (geq b lb)))))

(define (to-num b)
  (if b 1 0))

(define (rule-R v R t j w)
  (|| (&& (v j w) (= t j))
      (&& (R (- t 1) j w)
          (> j 0)
          (< j t)
          (> t 1))))

(define (sigma f)
  (lambda (w) (apply + (map f w))))

(define (rule-S v R t js ws)
  ((sigma
    (lambda (j)
      ((sigma
        (lambda (w)
          (* w
             (to-num
              (&& (rule-R v R t j w)
                  (< t N)
                  (<= 1 j)
                  (<= j t))))))
       ws)))
   js))

(define (rule-S-opt v R t js ws)
  (+ (* (to-num (&& (> t 1) (< t N))) (rule-S v R (- t 1) js ws))
     ((sigma
       (lambda (j)
         ((sigma
           (lambda (w)
             (* w
                (to-num
                 (&& (v j w)
                     (< t N)
                     (= t j)
                     (<= 1 j))))))
          ws)))
      js)))

(define (vt x w) (&& (= x 1) (= w 4)))
(define (Rt x y z) (&& (= x 1) (= y 1) (= z 4)))

(define-symbolic t tt j w integer?)
(define-symbolic v (~> integer? integer? boolean?))
(define-symbolic R (~> integer? integer? integer? boolean?))
(define js (range N))
(define ws (range N))

(assert (< t N))

(define sol (verify (assert (= (rule-S v R t js ws)
                               (rule-S-opt v R t js ws)))))

sol

(evaluate (rule-S v R t js ws) sol)
(evaluate (rule-S-opt v R t js ws) sol)
(evaluate (rule-S v R (- t 1) js ws) sol)
