#lang rosette

(define (I b)
  (if b 1 0))

(define-symbolic j w t k integer?)
(define-symbolic v (~> integer? integer? integer?))
(define-symbolic R (~> integer? integer? integer? integer?))

(verify (assert (= (* (* (v j w) (I (= t j)))
                      (* w (* (I (>= j 1))
                              (I (<= j t)))))
                   (* (* (v j w) (I (= t j)))
                      (* w (I (>= j 1)))))))

(verify (assert (= (* (* (v j w) (I (= (- t k) j)))
                      (* w (* (I (>= j 1))
                              (I (<= j (- t k))))))
                   (* (* (v j w) (I (= (- t k) j)))
                      (* w (I (>= j 1)))))))

(verify (assert (= (* (* (R (- t 1) j w)
                         (* (I (> t 1))
                            (I (< j t))))
                      (* w (* (I (>= j 1)) (I (<= j t)))))
                   (* (* (R (- t 1) j w)
                         (* (I (> t 1))
                            (I (< j t))))
                      (* w (I (>= j 1)))))))

(define-symbolic n a b integer?)
(assert (forall (list n a b) (=> (< n 1) (= 0 (R n a b)))))
(assert (>= k 0))
(assert (>= t 0))

(verify (assert (= (* (* (R (- (- t k) 1) j w)
                         (* (I (> (- t k) 1))
                            (I (< j (- t k)))))
                      (* w (* (I (>= j 1)) (I (<= j (- t k))))))
                   (* (* (R (- (- t k) 1) j w) w)
                      (* (* (I (<= j (- (- t k) 1)))
                            (I (<= 1 j)))
                         (I (> t 1)))))))
