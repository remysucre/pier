#lang rosette/safe

(define (to-num b)
  (if b 1 0))

;; (24) "a trivial identity": x = MIN_w 1_{x=w} * w
;; see apsp-verify/trivial


;; (49) 1_{j<t} = 1_{j<t-1} + 1_{j=t-1}

(define (check-49)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (to-num (< j t)))
  (define (rhs j t)
    (+ (to-num (< j (- t 1))) (to-num (= j (- t 1)))))
  ;; (verify (assert (< (lhs j t) (rhs j t))))
  ;; (verify (assert (> (lhs j t) (rhs j t))))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (50) 0 = 1_{j<s} * 1_{j=s}

(define (check-50)
  (define-symbolic j s integer?)
  (define (lhs j s)
    0)
  (define (rhs j s)
    (* (to-num (< j s)) (to-num (= j s))))
  (verify (assert (= (lhs j s) (rhs j s)))))

;; (51) 1_{j<t} * 1_{j<=t} = 1_{j<t}

(define (check-51)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (* (to-num (< j t)) (to-num (<= j t))))
  (define (rhs j t)
    (to-num (< j t)))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (52) 1_{j=t} * 1_{j<=t} = 1_{j=t}

(define (check-52)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (* (to-num (= j t)) (to-num (<= j t))))
  (define (rhs j t)
    (to-num (= j t)))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (53) 1_{j<t} = 1_{j<=t-1}

(define (check-53)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (to-num (< j t)))
  (define (rhs j t)
    (to-num (<= j (- t 1))))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (80) 1_{j<=t} = 1_{j-1<t}
(define (check-80)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (to-num (<= j t)))
  (define (rhs j t)
    (to-num (< (- j 1) t)))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (81) 1_{j<t} = 1_{j<=t} - 1_{j=t}

(define (check-81)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (to-num (< j t)))
  (define (rhs j t)
    (- (to-num (<= j t)) (to-num (= j t))))
  (verify (assert (= (lhs j t) (rhs j t)))))

;; (82) 1_{j<=t} = 1_{j-1<=t} - 1_{j-1=t}

(define (check-82)
  (define-symbolic j t integer?)
  (define (lhs j t)
    (to-num (<= j t)))
  (define (rhs j t)
    (- (to-num (<= (- j 1) t)) (to-num (= (- j 1) t))))
  (verify (assert (! (= (lhs j t) (rhs j t))))))

(check-49)
(check-50)
(check-51)
(check-52)
(check-53)
(check-80)
(check-81)
(assert (unsat? (check-82)))

;; Lattice lemmas from 6.7
