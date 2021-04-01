#lang rosette
(require "core/lib.rkt")

;; declare the types of relations and variables
(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

;; define helper functions
(def (weight w x z)
  ;; MIN_w . 1_{E(x,z,w)} + w.
  (sum w (* (I (E x z w)) w)))

;; recursive stratum
(rec (f R)
     (λ (x z w) (+
        ;; R(x,z,w) :- E(x,z,w).
        (I (E x z w))
        ;; R(x,z,w) :- R(x,y,w1), E(y,x,w2), w=w1+w2.
        (sum y (sum w1 (sum w2
           (* (* (I (R x y w1)) (I (E y z w2))) (I (= w (* w1 w2))))))))))

;; "return" stratum
(ret (g R)
     ;; S[x,z] = MIN_w . R(x,z,w) + w.
     (λ (x z) (sum w (* (R x z w) w))))

;; g(f(R))(x,z)
(define p (apply (g (f (hash-ref meta 'base))) (hash-ref meta 'g-args)))

(define (normalize p)
  (define in (serialize p rel var fun))
  (define out
    (with-output-to-string
      (λ () (parameterize
              ([current-input-port (open-input-string (~s in))])
              (system* semiring)))))
  (read (open-input-string (string-normalize-spaces out))))

(define prog (interpret (preprocess (deserialize (normalize p)) var rel fun)))

;; normalized (g R)
(define (g-R x z w) ; all variables in g
  (define vs (hash 'x x 'z z 'w w))
  (define (r x y z) `(I (R ,x ,y ,z)))
  (preprocess (deserialize (normalize ((g r) 'x 'z))) vs rel fun))

(postprocess (optimize prog g-R) var->symbol rel->symbol fun->symbol)
