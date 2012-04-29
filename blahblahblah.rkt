#lang racket

(begin-for-syntax
  
  ;; We can define a compile-time function:
  ;;
  ;; repeat-three: syntax -> syntax
  (define (repeat-three stx)
    (syntax-case stx ()
      [(_ thing)
       #'(begin
           thing thing thing)])))
  

;; And hook it up to the macro expander:
(define-syntax blahblahblah repeat-three)


;; We can look at the compile-time binding for the thing connected to
;; the id blah-blah-blah by using syntax-local-value
(begin-for-syntax
 (printf "blahblahblah is connected to: ~s\n"
         (syntax-local-value (syntax blahblahblah))))
 

(define (f)
  (blahblahblah (display "blah"))
  (newline))

(f)