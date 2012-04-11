#lang racket

(provide def outer)

(require racket/stxparam
         racket/splicing)

(require (for-syntax racket/list))

;; We represent both the inside and outside of a scope
;; by holding both respectively in these two parameters.
(define-syntax-parameter current-insides '())
(define-syntax-parameter current-outsides '())
;; Unlike the first version, we keep a stack of these scopes
;; around for inspection.


;; We revise def to maintain both.
(define-syntax (def stx)
 (syntax-case stx ()
   [(_ (name args ...) body ...)
    (with-syntax ([fun-stx stx])
      #'(splicing-syntax-parameterize
         ([current-outsides
           (cons #'fun-stx
                 (syntax-parameter-value #'current-outsides))])
         (define (name args ...)
           (splicing-syntax-parameterize
            ([current-insides (cons #'fun-stx
                                    (syntax-parameter-value #'current-insides))])
            body ...))))]))


(begin-for-syntax
 ;; find-scope: identifier (listof syntax) (listof syntax) positive-integer -> (U syntax #f)

 ;; Given an identifier, we now need a function to help
 ;; us find the innermost scope that still can refer to id.
 ;; Once we do so, we can then provide the lexical information
 ;; outside of that scope.
 (define (find-scope id outsides insides n)
   (cond
     [(empty? insides)
      ;; FIXME: we really should raise a good syntax error at this point.
      #f]
     ;; If an instance of an an identifier in the inside of a scope
     ;; matches the id we're searching, then we just found an appropriate
     ;; scope.
     [(free-identifier=? id
                         (datum->syntax (first insides) (syntax-e id)))
      (cond
        [(= n 1)
         (first outsides)]
        [else
         ;; If we need to skip more than one scope, we restructure the
         ;; searched id so that it's as if we're looking for it from
         ;; the perspective of the outside.
         (find-scope (datum->syntax (first outsides) (syntax-e id))
                     (rest outsides)
                     (rest insides)
                     (sub1 n))])]
     [else
      (find-scope id (rest outsides) (rest insides) n)])))


(define-syntax (outer stx)
 (syntax-case stx ()
   [(_ n id)
    (and (exact-positive-integer? (syntax-e #'n))
         (identifier? #'id))
    (let ()
      (define found-binding
        (find-scope #'id
                    (syntax-parameter-value #'current-outsides)
                    (syntax-parameter-value #'current-insides)
                    (syntax-e #'n)))
      (datum->syntax found-binding
                     (syntax-e #'id)
                     #'id))]
   [(_ id)
    (identifier? #'id)
    #'(outer 1 id)]))


;; (define-syntax m
;;  (syntax-rules ()
;;    ((_ val)
;;     (let ()
;;       (def (h x) (* x val))   ;; slight change to make even more devious
;;       (h 2)))))


;; (define x 42)

;; (def (g x)
;;  (def (h x)
;;    (m (outer 1 x)))
;;  (h 3))