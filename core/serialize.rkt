#lang rosette

(provide (all-defined-out))

(define (serialize e rel var fun)
  (define (ser e)
    (match e
      [(? (curry hash-has-key? var) e) e]
      [`(sum ,v ,b) `(sum ,v ,(ser b))]
      [`(,f ,es ...) (cond [(hash-has-key? rel f) `(rel ,f ,@(map ser es))]
                           [else `(,f ,@(map ser es))])]
      [n n]))
  (ser e))

(define (make-pattern e)
  (define (mark s)
    (string->symbol (string-append "?" (symbol->string s))))
  (match e
    [(? symbol? e) (mark e)]
    [`(sum ,v ,b) `(sum ,(mark v) ,(make-pattern b))]
    [`(,o ,xs ...) `(,o ,@(map make-pattern xs))]
    [_ e]))

(define (deserialize e)
  (match e
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

(define (extract rw e)
  (define out
    (with-output-to-string
      (λ () (parameterize
              ([current-input-port (open-input-string (~s e))])
              (system* semiring "extract" rw)))))
  (deserialize (read (open-input-string (string-normalize-spaces out)))))
