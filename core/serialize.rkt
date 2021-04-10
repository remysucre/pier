#lang rosette

(provide (all-defined-out))

(define (serialize e)
  (match e
    [`(,o ,es ...) `(,o ,@(map serialize es))]
    [_ e]))

(define (deserialize e)
  (match e
    [`(,f ,es ...) `(,f ,@(map deserialize es))]
    [_ e]))

(define (make-pattern e)
  (match e
    [`(rel ,r ,xs ...) `(rel ,r ,@(map make-pattern xs))]
    [`(,o ,xs ...) `(,o ,@(map make-pattern xs))]
    [(? symbol? e) (~a '? e)]
    [_ e]))

(define semiring-path "/home/remywang/projects/semiring/target/release/semiring")

(define (semiring e . args)
  (define out
    (with-output-to-string
      (λ () (parameterize
              ([current-input-port (open-input-string (~s e))])
              (apply system* (cons semiring-path args))))))
  (deserialize (read (open-input-string (string-normalize-spaces out)))))

(define (normalize e) (semiring (serialize e)))
(define (extract rw e) (semiring e "extract" rw))
