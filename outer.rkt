#lang racket
(require racket/splicing
         (for-syntax syntax/strip-context))

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([outside-context stx]
                   [outer-keyword (datum->syntax stx 'outer)])
       #'(splicing-let-syntax ([outer-keyword
                                (lambda (an-stx)
                                  (syntax-case an-stx ()
                                    [(_ outer-expr)
                                     (replace-context #'outside-context #'outer-expr)]))])
            (define (name args ...)
               body ...)))]))

(define-syntax (outer stx)
  (raise-syntax-error #f "Shouldn't be used outside the context of a def\n" stx))


(module+ test

  (require rackunit)

  (let ()
    (def (f x) 
      (def (g x) (* (outer x) x))
        (g 4))
    (check-expect (f 2) 8)))
