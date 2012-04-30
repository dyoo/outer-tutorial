#lang racket/base
(require racket/splicing
         (for-syntax racket/base
                     syntax/strip-context))

(provide def outer)

(begin-for-syntax
 ;; We keep a private version of "outer".  It's important that it be not
 ;; accessible outside this module.
 (define private-outer-id (string->uninterned-symbol "private-outer-id")))


(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([outer-keyword (datum->syntax stx private-outer-id)])

       ;; I want the outer keyword bound out here, because if the
       ;; user defines an 'outer' as a parameter or a function name, I'd
       ;; still like that binding to shadow our own.
       #`(splicing-let-syntax ([outer-keyword
                                (lambda (an-stx)
                                  (syntax-case an-stx ()
                                    [(o outer-expr)
                                     (replace-context #'#,stx #'outer-expr)]

                                    ;; For convenience, also support using integers
                                    ;; here.
                                    [(o k outer-expr)
                                     (and (exact-positive-integer? (syntax->datum #'k))
                                          (= (syntax->datum #'k) 1))
                                     (syntax/loc an-stx (o outer-expr))]

                                    [(o k outer-expr)
                                     (and (exact-positive-integer? (syntax->datum #'k))
                                          (> (syntax->datum #'k) 1))
                                     (with-syntax ([k-1 (sub1 (syntax->datum #'k))])
                                       (syntax/loc an-stx (o k-1 (o outer-expr))))]))])

            #,(syntax/loc stx
                (define (name args ...)
                  body ...))))]))

(define-syntax (outer stx)
  (cond
   [(eq? (identifier-binding (datum->syntax stx private-outer-id)) #f)
    (raise-syntax-error #f "Shouldn't be used outside the context of a def\n" stx)]
   [else
    (syntax-case stx ()
      [(_ body ...)
       (with-syntax ([redirected-outer (datum->syntax stx private-outer-id stx)])
         (syntax/loc stx
           (redirected-outer body ...)))])]))
        
