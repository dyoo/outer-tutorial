#lang racket
(require racket/block)

(provide def)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([outside-context stx]
                   [outer (datum->syntax stx 'outer)])
       #'(define name
           (block

            (define-syntax (outer stx)
              (syntax-case stx ()
                [(_ sub-expr)
                 (datum->syntax #'outside-context (syntax->datum #'sub-expr))]))

            (lambda (args ...)
              body ...))))]))




(module+ test
  (def (f x) (def (g x) (* (outer x) x)) (g 4))
  (f 2))