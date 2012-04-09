#lang racket


(define-syntax (phase-1 stx)
  (syntax-case stx ()
    [(_ (d (f i)
           (p2 (mul a b))))
     (begin 
       (displayln (identifier-binding #'a))
       #'(d (f i)
            (p2 (mul a b))))]))

(define-syntax (phase-2 stx)
  (syntax-case stx ()
    [(_ (mul a b))
     (begin
       (displayln (identifier-binding #'a))
       #'(mul a b))]))
     
(phase-1 
 (define (f x)
   (phase-2
    (* x x))))