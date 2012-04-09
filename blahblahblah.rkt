#lang racket

(begin-for-syntax
  
  ;; We can define a compile-time function:
  ;;
  ;; repeat-three: syntax -> syntax
  (define (repeat-three stx)
    (syntax-case stx ()
      [(_ thing)
       #'(begin thing thing thing)])))
  

;; And hook it up to the macro expander:
(define-syntax blahblahblah repeat-three)


(define (f)
  (blahblahblah (display "blah"))
  (newline))

(f)