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
(define (r x y z) `(R ,x ,y ,z))

;; TODO insert quotes automatically
;; interpret arguments, symbolize others
;; f(R)(x,y,w)
(define (f R x z w)
  `(+ (I (E ,x ,z ,w))
      (sum y (sum w1 (sum w2 (* (* (I ,(R x 'y 'w1))
                                   (I (E y ,z w2)))
                                (I (= ,w (* w1 w2)))))))))

;; g(S)(x,z)
(define (g S x z)
  `(sum w (* ,(S x z 'w) w)))

(define expr (serialize (g (curry f r) 'x 'z) rel var fun))

(define o (open-output-file #:exists 'replace "/home/remywang/projects/pier/temp"))
(write expr o)
(close-output-port o)

(define semiring-out (string-join (string-split (with-output-to-string (λ() (system "/home/remywang/projects/semiring/target/release/semiring < /home/remywang/projects/pier/temp"))) "\n")))

(define normalized (read (open-input-string semiring-out)))

(define p (interpret (preprocess (deserialize normalized) var rel fun)))

;; normalized (g R)
(define (ng w) (op-sum w (op-* w (op-I (op-rel R (list x y w))))))

(optimize p ng)
