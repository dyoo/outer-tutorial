#lang racket

;; Here, we make sure that, even when prefixed, outer does the right thing.

(require (rename-in "outer.rkt" [outer otter])
         rackunit)



(let ()
  (def (f x) 
    (def (g x) (* (otter x) x))
      (g 4))
  (check-equal? (f 2) 8))



(define-syntax m1
(syntax-rules ()
  ((_ val)
   (let ()
     (def (h x) 
       val)
     (h 2)))))

(define-syntax m2
(syntax-rules ()
  ((_ val)
   (let ()
     (def (h x)
       (def (g x)
         val)
       (g x))
     (h 2)))))


(define-syntax (check-good stx)
 (syntax-case stx ()
   [(_ body ...)
    #'(check-equal? (let () body ...) 'good)]))
   


(check-good (def (g x)
             (def (h x)
               (m2 (otter x)))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (m1 (otter x)))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (otter 1 x)))
               (k 'bad))
             (h 'good))
           (g 'bad))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (otter (otter x))))
               (k 'bad))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m2 (otter 1 x)))
               (k 'bad))
             (h 'good))
           (g 'bad))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m2 (otter 2 x)))
               (k 'bad))
             (h 'bad))
           (g 'good))



(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m2 (otter (otter x))))
               (k 'bad))
             (h 'bad))
           (g 'good))



;; nesting m1 and m2 should still not break otter.

(check-good (def (g x)
             (def (h x)
               (m1 (m2 (otter x))))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (m1 (m1 (otter x))))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (m1 (otter 1 x))))
               (k 'bad))
             (h 'good))
           (g 'bad))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (m1 (otter 2 x))))
               (k 'bad))
             (h 'bad))
           (g 'good))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (m2 (otter 1 x))))
               (k 'bad))
             (h 'good))
           (g 'bad))


(check-good (def (g x)
             (def (h x)
               (def (k x)
                 (m1 (m2 (otter 2 x))))
               (k 'bad))
             (h 'bad))
           (g 'good))



(define-syntax m3
 (syntax-rules ()
  ((_ id val)
   (let ()
     (def (h id)
       (def (g id)
         val)
       (g id))
     (h 2)))))

(check-equal? (let ()
                (def (g x)
                  (def (h x)
                     (m3 x (otter x)))
                  (h 5))
                (g 17))
              17)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (let ()
                (def (g otter)
                  otter)
                (g 'test))
              'test)



#;(check-equal? (let ()
                (def (g otter)
                  (def (h x)
                    (otter otter))
                  (h otter))            
                (g 'test-again))
              'test-again)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ()
  (define-syntax m3
    (syntax-rules ()
      ((_ id val)
       (let ()
         (def (h id)
           (def (g id)
             val)
           (g id))
         (h 2)))))

  (def (g x)
    (def (h x)
      (m3 x (otter x)))
    (h 5))

  (check-equal? (g 4) 4))
