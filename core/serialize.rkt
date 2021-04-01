#lang rosette

(provide (all-defined-out))

(define (serialize e rel var fun)
  (define (ser e)
    (match e
      [(? symbol? e) `(var ,e)]
      [`(sum ,v ,b) `(sum ,v ,(ser b))]
      [`(,f ,es ...) (cond [(hash-has-key? rel f) `(rel ,f ,@(map ser es))]
                           [else `(,f ,@(map ser es))])]
      [n n]))
  (ser e))

(define (deserialize e)
  (match e
    [`(var ,x) x]
    [`(rel ,r ,vs ...) `(,r ,@(map deserialize vs))]
    [`(sum ,v ,b) `(sum ,v ,(deserialize b))]
    [`(,f ,es ...) `(,f ,@(map deserialize es))]
    [n n]))

(define semiring "/home/remywang/projects/semiring/target/release/semiring")

(define (normalize p var rel fun)
  (define in (serialize p rel var fun))
  (define out
    (with-output-to-string
      (λ () (parameterize
              ([current-input-port (open-input-string (~s in))])
              (system* semiring)))))
  (deserialize (read (open-input-string (string-normalize-spaces out)))))
