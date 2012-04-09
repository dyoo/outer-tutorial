#lang racket

(define-syntax (blahblahblah stx)
  (syntax-case stx ()
    [(_ thing)
     #'(begin thing thing thing)]))


(define (f)
  (blahblahblah (display "blah"))
  (newline))

(f)