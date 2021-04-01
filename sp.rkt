#lang rosette
(require "core/lib.rkt")

;; declare the types of relations and variables
(decl rel E R (~> id? id? int? bool?))
(decl var x y z id?)
(decl var w w1 w2 int?)

;; define helper functions
(def (weight w x z)
  (sum w (* w (I (E x z w)))))

;; TODO infer base rel automatically
(define (base x y w) `(R ,x ,y ,w))

;; TODO insert quotes automatically
;; interpret arguments, symbolize others
;; f(R)(x,y,w)
(define (f R)
  (lambda (x z w)
    `(+ (I (E ,x ,z ,w))
        (sum y
             (sum w1
                  (sum w2
                       (* (* (I ,(R x 'y 'w1))
                             (I (E y ,z w2)))
                          (I (= ,w (* w1 w2))))))))))

;; g(S)(x,z)
(define (g S)
  (lambda (x z) `(sum w (* ,(S x z 'w) w))))

;; g(f(R))(x,z)
(define p ((g (f base)) 'x 'z))

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
