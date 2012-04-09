#lang racket
(require racket/stxparam
         racket/splicing)

(define-syntax-parameter current-def #f)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([fun-stx stx])
     #'(splicing-syntax-parameterize ([current-def #'fun-stx])
                                     (define (name args ...)
                                       body ...)))]))
(define-syntax (bad-def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([fun-stx stx])
       #'(define (name args ...)
           (splicing-syntax-parameterize ([current-def #'fun-stx])
             body ...)))]))

(define-syntax (outer stx)
  (syntax-case stx ()
    [(_ id)
     (datum->syntax (syntax-parameter-value #'current-def)
                    (syntax-e #'id)
                    stx)]))


(def (f x) 
  (def (g x) (* (outer x) x))
  (g 4))

(f 2)



(bad-def (k x) 
  (bad-def (g x) (* (outer x) x))
  (g 4))
(k 2)