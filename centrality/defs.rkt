#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt")

(provide (all-defined-out))

;; symbolics

;; (define-symbolic E (~> integer? integer? boolean?))
;; (define-symbolic sigma (~> integer? integer? real?))
;; (define-symbolic D (~> integer? integer? real?))
;; (define-symbolic s t u v integer?)

;; (define-symbolic sig (~> integer? real? real?))
;; (define-symbolic diiv (~> real? real? real?))
;; (define-symbolic inv (~> real? real?))

(define-symbolic E (~> integer? integer? boolean?))
(define-symbolic sigma (~> integer? integer? integer?))
(define-symbolic D (~> integer? integer? integer?))
(define-symbolic s t u v integer?)

(define-symbolic sig (~> integer? integer? integer?))
(define-symbolic inv (~> integer? integer?))

(define (I b) (if b 1 0))

(define (sigma-3 s v t)
  (op-* (op-* (rel-sigma s v) (rel-sigma v t))
        (op-I (op-eq? (rel-D s t)
                      (op-+ (rel-D s v)
                            (rel-D v t))))))

(define (delta s v t)
  (op-* (op-sigma s v t) (op-inv (rel-sigma s t))))

(define vars
  (list (cons 's s)
        (cons 't t)
        (cons 'u u)
        (cons 'v v)))
