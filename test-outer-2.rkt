#lang racket

(require "outer-2.rkt"
         rackunit)
;
;(define-syntax m1
; (syntax-rules ()
;   ((_ val)
;    (let ()
;      (def (h x) 
;        val)
;      (h 2)))))
;
;(define-syntax m2
; (syntax-rules ()
;   ((_ val)
;    (let ()
;      (def (h x)
;        (def (g x)
;          val)
;        (g x))
;      (h 2)))))
;
;
;(define-syntax (check-good stx)
;  (syntax-case stx ()
;    [(_ body ...)
;     #'(check-equal? (let () body ...) 'good)]))
;    
;
;
;(check-good (def (g x)
;              (def (h x)
;                (m2 (outer x)))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (m1 (outer x)))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (outer 1 x)))
;                (k 'bad))
;              (h 'good))
;            (g 'bad))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (outer 2 x)))
;                (k 'bad))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m2 (outer 1 x)))
;                (k 'bad))
;              (h 'good))
;            (g 'bad))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m2 (outer 2 x)))
;                (k 'bad))
;              (h 'bad))
;            (g 'good))
;
;
;
;
;
;;; nesting m1 and m2 should still not break outer.
;
;(check-good (def (g x)
;              (def (h x)
;                (m1 (m2 (outer x))))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (m1 (m1 (outer x))))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (m1 (outer 1 x))))
;                (k 'bad))
;              (h 'good))
;            (g 'bad))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (m1 (outer 2 x))))
;                (k 'bad))
;              (h 'bad))
;            (g 'good))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (m2 (outer 1 x))))
;                (k 'bad))
;              (h 'good))
;            (g 'bad))
;
;
;(check-good (def (g x)
;              (def (h x)
;                (def (k x)
;                  (m1 (m2 (outer 2 x))))
;                (k 'bad))
;              (h 'bad))
;            (g 'good))







(define-syntax m3
 (syntax-rules ()
  ((_ id val)
   (let ()
     (def (h id)
       (def (g id)
         val)
       (g id))
     (h 2)))))


;(def (g x)
; (def (h x)
;   (m3 x (outer x)))
; (h 5))
