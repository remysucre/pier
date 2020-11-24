#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/destruct)  ; provides `destruct`
; Tell Rosette we really do want to use integers.
(current-bitwidth #f)

; Syntax for our simple DSL
(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)

; Interpreter for our DSL.
; We just recurse on the program's syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))

; Create an unknown expression -- one that can evaluate to several
; possible values.
(define (??expr terminals depth)
  (if (= 0 depth)
      (apply choose* terminals)
      (apply choose*
             (append (list (plus (??expr terminals (- depth 1))
                                 (??expr terminals (- depth 1)))
                           (square (??expr terminals (- depth 1))))
                     terminals))))

; Create a sketch representing all programs of the form (plus ?? ??),
; where the ??s are unknown expressions created by ??expr.
(define-symbolic x integer?)
(define sketch (??expr (list x) 3))

(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert (= (interpret sketch) (interpret (mul 8 x))))))


; Substitute the bindings in M into the sketch to get back the
; synthesized program.
(evaluate sketch M)
