#lang racket/base
(require racket/splicing
         racket/stxparam
         (for-syntax racket/base
                     syntax/strip-context))

(provide def outer)

(begin-for-syntax
 (define internal-outer-keyword (car (generate-temporaries #'(outer)))))


(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([outside-context stx]
                   [internal-outer-keyword
                    (datum->syntax stx (syntax->datum internal-outer-keyword))])
       ;; I want the outer keyword bound out here, because if the
       ;; user defines an 'outer' as a parameter or a function name, I'd
       ;; still like that binding to shadow our own.
       #'(splicing-let-syntax ([internal-outer-keyword
                                (lambda (an-stx)
                                  (syntax-case an-stx ()
                                    [(o outer-expr)
                                     (replace-context #'outside-context #'outer-expr)]

                                    ;; For convenience, also support using integers
                                    ;; here.
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


(define-syntax outer
  (lambda (stx)
    (syntax-case stx ()
      [(_ body ...)
       #`(#,(replace-context stx internal-outer-keyword)  body ...)])))
