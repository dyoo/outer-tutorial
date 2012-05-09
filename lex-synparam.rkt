#lang racket/base
(require (for-syntax racket/base))

;; Lexical syntax parameters
;; They act somewhat like syntax parameters, except the keyword binds lexically
;; rather than purely syntactically.

(begin-for-syntax
 (struct lexparam (id default-value private-id)
         #:property prop:procedure
         (lambda (self stx)
           (cond
            [(eq? (identifier-binding (datum->syntax stx (lexparam-private-id self)))
                  #f)
             ((lexparam-default-value self) stx)]
            [else
             (syntax-case stx ()
               [(_ body (... ...))
                (with-syntax ([redirected-id (datum->syntax stx (lexparam-private-id self) stx)])
                  (syntax/loc stx
                    (redirected-id body (... ...))))])]))))



(define-syntax (define-lexical-syntax-parameter stx)
  (syntax-case stx ()
    [(_ id default-value)
     (with-syntax ([string-id (symbol->string (syntax-e #'id))])
       (syntax/loc stx
         (define-syntax id
           (lexparam 'id
                     default-value
                     (string->uninterned-symbol string-id)))))]))

(define-syntax (lexical-syntax-parameterize stx)
  (syntax-case stx ()
    [(_ ([id value] ...) body ...)
     (for/and ([s (syntax->list #'(id ...))])
         (lexparam? (syntax-local-value s)))
     (with-syntax ([(scoped-id ...)
                    (for/list ([s (syntax->list #'(id ...))])
                      (datum->syntax stx (lexparam-private-id (syntax-local-value s))))])
       #'(splicing-let-syntax ([scoped-id value] ...)
            body ...))]))