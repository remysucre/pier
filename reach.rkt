#lang rosette

(require "core/ops.rkt"
         "core/process.rkt"
         "core/interpret.rkt"
         "core/grammar.rkt")

(require rosette/lib/destruct
         rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax)   ; provides `??`

;; 1. declare relations and types

(struct rel-E (x y) #:transparent)
(define-symbolic E (~> integer? integer? boolean?))

(struct rel-R (x y) #:transparent)
(define-symbolic R (~> integer? integer? boolean?))

;; 3. define types for variables

(define-symbolic x y z integer?)

;; 4. map symbols to declared constructs

(define vars
  (list (cons 'x x)
        (cons 'y y)
        (cons 'z z)))

;; 5. extend the interpreter over relations and macros

(define (interp-prog p)
  (define (interp p)
    (interpret interp-prog vars p))
  (destruct p
    [(rel-E x y) (E (interp x) (interp y))]
    [(rel-R x y) (R (interp x) (interp y))]
    [p p]))

;; INPUT

(define S-29
  (op-|| (rel-E 1 'y)
        (op-exists 'z
                   (op-&& (rel-R 1 'z)
                         (rel-E 'z 'y)))))

(define (??v) (choose* 'x 'y 'z 1))
(define (??op) (choose* op-|| op-&&))

(define (??term depth)
  (gen-term depth (??v)
            (list (rel-E (??v) (??v))
                  (rel-R (??v) (??v)))
            ??op))

(define sketch
  (op-|| (??term 0)
        (op-exists (??v)
                   (op-&& (rel-R 1 (??v))
                         (??term 0)))))

(define M
  (synthesize
   #:forall (list E R x y z exist)
   #:guarantee (assert (eq? (interpret interp-prog vars sketch) (interpret interp-prog vars S-29)))))

(evaluate sketch M)
