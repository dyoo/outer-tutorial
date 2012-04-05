#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/stxparam
                     racket/splicing))

@(define my-eval (make-base-eval))


@title{Expanding the outer boundaries: a Racket macro toy}


Most programming languages provide a notion of binding and scope.
If we have a definition like:
@codeblock|{
(define (f x)
  ...)
}|
then within the scope of the function's body, any references to
@racket[x] refer to the binding to @racket[f]'s first argument.

... well, except that's not always true!  In particular, we might
override or @emph{shadow} a binding by setting up a new one:

@codeblock|{
(define (f x)
  (define (g)
    (define x 'objection!)   ;; Overruled!
    x)
  (g))
}|

Within the body of @racket[f], the internal definition of @racket[x]
in @racket[g]'s body sets up a binding that overrides the one from the
function argument.


Functions can establish their own internal scopes, and they can be
nested within one another.  These scopes form nested boundaries.

@codeblock|{
(define (f x)
  (define (g x)
     (define (h x)
       (... 
        x        ;; within here, x refers to h's x. 
        ...))
     ...
     x           ;; within here, x refers to g's x. 
     ...)
  ...
  x              ;; within here, x refers to f's x. 
  ...)
}|
     
Now here's a demented question: can we poke a hole through these
contours?  That is, would it be possible to say something like this?
@codeblock|{
(define (f x)
  (define (g x)
    ...
    (outer x)    ;; refers to f's x?
    ...)
  ...)
}|
where @racket[outer] allows us to get at the binding outside of @racket[g]?


The following mini-tutorial shows how we might poke a lexical scoping
hole in a controlled way, using Racket's macro system.  The technique
we'll use is one that cooperates with Racket's compiler at
compile-time, so that we won't impose any run-time penalty.

@section{Defining @racket[def]}
As the thought experiment suggests, let's say that the boundaries will
be at the outskirts of a function definition.  In fact, let's make
these boundaries a bit more explicit, by introducing our own
@racket[def] form.  It'll works similarly to @racket[define].

@codeblock|{
#lang racket

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (syntax/loc stx
       (define (name args ...) body ...))]))
}|

@racket[def] gives us a function definition syntax that, at the
moment, works like @racket[define].

@(begin
(my-eval '(begin (require racket/stxparam (for-syntax racket/base) racket/splicing)
                 (define-syntax-parameter current-def #f)
                 (define-syntax (def stx)
                   (syntax-case stx ()
                     [(_ (name args ...) body ...)
                      (quasisyntax/loc stx
                        (splicing-syntax-parameterize ([current-def #'#,stx])
                                                      (define (name args ...)
                                                        body ...)))])))))

@interaction[#:eval my-eval
(def (f x) (* x x))
(f 3)
]

During compilation time, information about lexical scope lives within
the syntax objects that our macros process during compilation.  In
fact, whenever we use @racket[datum->syntax] to take some inert piece
of data and turn it into syntax, the first argument that it takes is a
@emph{source} of lexical information.


We know we want to amend @racket[def] so that it stores the @racket[stx] object...
@codeblock|{
     ;; in the def macro:
     (syntax/loc stx
       ;; Somehow, we want to hold onto the stx syntax object and make
       ;; it available during the compilation of body around here...
       (define (name args ...)
         body ...))
}|


@margin-note{We might use @racket[syntax-parameterize], except that if
we do so, we interfere with how @racket[define] needs to be used in a
definition context, which @racket[syntax-parameterize] does not provide.}
... and we want this information to be accessible when the body of the
function is being compiled.  This is a job for the
@racket[splicing-syntax-parameterize] form, which allows us to
maintain this kind of compile-time information during compilation and
share it as we're expanding the body.

@codeblock|{
#lang racket

(require racket/stxparam        ;; syntax parameters are defined in the
         racket/splicing)       ;; racket/stxparam library and
                                ;; racket/splicing

;; Let's make a compile-time parameter called current-def that
;; remember the innermost def that's currently being compiled.
(define-syntax-parameter current-def #f)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (quasisyntax/loc stx
       (splicing-syntax-parameterize ([current-def #'#,stx])
         (define (name args ...)
            body ...)))]))
}|


@subsection{Timing is everything}
@;
@; Not quite satisfied with this explanation yet.  Need confirmation.
@;
Note that we copy the lexical information outside the @racket[define]:
this is intentional.  If we do it within,
@codeblock|{
(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (quasisyntax/loc stx
       (define (name args ...)
         (splicing-syntax-parameterize ([current-def #'#,stx])
            body ...)))]))
}|
then we end up accidently copying the lexical information too late,
because it has been enriched with the binding information for its
variables.  We need to take care to exclude that information.


@section{The @racket[outer] limits}

Now that this version of @racket[def] holds on to the currently
expanding definition, other compile-time macros that run in the
context of the body's expansion can access that outside lexical scope.
Let's write @racket[outer] now.  Given something like @racket[(outer
some-id)], we take @racket[some-id], rip the syntaxness out of the
symbol with @racket[syntax-e], and surgically create a new syntax with
the lexical information of the outer scope.
@margin-note{In production code, we'd probably use the @racket[replace-context] function from the @racketmodname[syntax/strip-context] library instead.}
@codeblock|{
(define-syntax (outer stx)
  (syntax-case stx ()
    [(_ id)
     (datum->syntax (syntax-parameter-value #'current-def)
                    (syntax-e #'id)
                    stx)]))
}|


@(my-eval '(define-syntax (outer stx)
             (syntax-case stx ()
               [(_ id)
                (datum->syntax (syntax-parameter-value #'current-def)
                               (syntax-e #'id)
                               stx)])))

And now we can try this out:
@interaction[#:eval my-eval
(def (f x) 
  (def (g x) (* (outer x) x))
  (g 4))

(f 2)
]

Hurrah!




@section{Source code}
@filebox["outer.rkt"]{
@codeblock|{
#lang racket

(require racket/stxparam
         racket/splicing)

(define-syntax-parameter current-def #f)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (quasisyntax/loc stx
       (splicing-syntax-parameterize ([current-def #'#,stx])
         (define (name args ...)
           body ...)))]))

(define-syntax (outer stx)
  (syntax-case stx ()
    [(_ id)
     (datum->syntax (syntax-parameter-value #'current-def) (syntax-e #'id))]))

;; Example code
(def (f x) 
  (def (g x) (* (outer x) x))
  (g 4))

(f 2)
}|
}