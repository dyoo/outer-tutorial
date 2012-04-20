#lang racket
(require racket/splicing
         (for-syntax syntax/strip-context))

(provide def outer)


(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([outside-context stx]
                   [outer-keyword (datum->syntax stx 'outer)])
       #'(splicing-let-syntax ([outer-keyword
                                (lambda (an-stx)
                                  (syntax-case an-stx ()
                                    [(o outer-expr)
                                     (replace-context #'outside-context #'outer-expr)]

                                    [(o k outer-expr)
                                     (and (exact-positive-integer? (syntax->datum #'k))
                                          (= (syntax->datum #'k) 1))
                                     #'(o outer-expr)]

                                    [(o k outer-expr)
                                     (and (exact-positive-integer? (syntax->datum #'k))
                                          (> (syntax->datum #'k) 1))
                                     (with-syntax ([k-1 (sub1 (syntax->datum #'k))])
                                        #'(o k-1 (o outer-expr)))]))])

            (define (name args ...)
               body ...)))]))

(define-syntax (outer stx)
  (raise-syntax-error #f "Shouldn't be used outside the context of a def\n" stx))
