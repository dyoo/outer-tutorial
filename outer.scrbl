#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/stxparam
                     racket/splicing))

@(define my-eval (make-base-eval))


@; I want to make Portal 2 jokes in here.  That's essentially what
@; we're setting up: a portal for lexical scope to poke through.
@;
@; Something from Mass Effect would also be nice.


@title{Expanding the boundaries of @tt{outer} space}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


Most programming languages provide a notion of binding and scope.  We
can bind an name somewhere, and then refer to it in some region of
a program.  For example, if we have a definition like:
@codeblock|{
(define (f x)
  ...)
}|
the function's header binds @racket[x] as one of the arguments.
Within the scope of the function's body, any references to
@racket[x] refer to the binding of @racket[f]'s first argument.

... well, except this isn't always true!  In particular, we might
override or @emph{shadow} a binding by setting up a new one:

@codeblock|{
(define (f x)
  (define (g)
    (define x 'objection!)   ;; Overruled!
    x)                       ;; This is g's x, not f's.

  (g))
}|

Within the body of @racket[f], the internal definition of @racket[g]
in sets up a binding for @racket[x] that blankets the one from
@racket[f]'s.



Functions can establish their own internal scopes, and they can be
nested within one another:
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


Now here's a loony question: can we poke a hole through these
contours?  That is, would it be possible to say something like the following:
@codeblock|{
(define (f x)
  (define (g x)
    ...
    (outer x)    ;; can we make this refer to f's x?
    ...)
  ...)
}|
where @racket[outer] allows us to get at the binding outside of @racket[g]?


The following tutorial shows how we might poke lexical scoping portals
into our programs.  The techniques we'll show here are those that
cooperate with Racket's compiler, so that we won't impose any run-time
penalty.  The tutorial is meant to be self-contained, and expects only
moderate Racket knowledge (functions, modules, structures).  Please
let me know if you have any suggestions or comments.



@section{A brief macro tutorial}

A common perception about Racket is that it's an interpreter-based
implementation, since it has a REPL and can
@link["http://docs.racket-lang.org/guide/reflection.html"]{dynamically
evaluate} programs.  However, this perception is not quite correct.

Racket does use a compiler to translate programs into an
@link["http://docs.racket-lang.org/raco/decompile.html#(part._.Bytecode_.Representation)"]{internal
bytecode format} for optimization and ease of execution.  However,
Racket hides this compiler from most users because, under normal
usage, Racket first quietly runs its compiler across a program, and
then immediately executes the compiled in-memory bytecode.  In fact,
tools like
@link["http://docs.racket-lang.org/raco/make.html"]{@tt{raco make}}
allow us to launch the compilation phase up front and save the
bytecode to disk.  If we use @tt{raco make}, then program execution
can pick up immediately from the on-disk bytecode.)


One thing that makes Racket an interesting language is that it allows
its users to hook expressions and functions into the compiler, so that
these compile-time expressions get evaluated and called during the
compilation phase.  And unlike a purely textual pre-processor, these
compile-time expressions can use the full power of Racket.

As a quick example, say that we have the following program:
@filebox["date-at-compile-time.rkt"]{
@codeblock|{
#lang racket
(require (for-syntax racket/date))
(begin-for-syntax
  (printf "This program is being compiled at ~a\n" 
          (date->string (current-date))))
}|
}


@margin-note{If we run this from DrRacket, we may see somewhat more unusual output, because
DrRacket can apply several program transformations  that may
cause @filepath{date-at-compile-time.rkt} to be compiled multiple times.}
The main element in this program, the use of
@racket[begin-for-syntax], declares an expression that should execute at compile-time.
Let's see what happens when we run this program from a command-line shell:

@nested[#:style 'inset]{
@verbatim|{
$ racket date-at-compile-time.rkt 
This program is being compiled at Monday, April 9th, 2012
$
}|
}

This output supports the idea that, under normal circumstances,
Racket interposes a compilation phase since it doesn't see any stored
bytecode on disk.

Let's compile the program, using @tt{raco make}:
@nested[#:style 'inset]{
@verbatim|{
$ raco make date-at-compile-time.rkt 
This program is being compiled at Monday, April 9th, 2012
$
}|
}

What is different is that bytecode has been written to disk, under a
@filepath{compiled} subdirectory.  Now let's try running the program
with the bytecode having just been saved to disk:

@nested[#:style 'inset]{
@verbatim|{
$ racket date-at-compile-time.rkt 
$ 
}|
}

It looks like it's not doing anything.  That's because it's not doing
anything.

The point is that our Racket programs can express both run-time and
compile-time computations, and they run in distinct phases.




@subsection{Macros are compile-time functions}

One of the main applications of compile-time computation is to rewrite
programs from one form to another.  Racket's compiler has a built-in
@emph{expander} that uses compile-time functions to rewrite a program.
Racket's expander is open to extension by letting us associate a
compile-time function to a name; we call such compile-time functions
``@emph{macros}''.  When the expander sees a name that's associated to
a macro, it applies that macro on a selected portion of the program
and replaces that portion with the value returned from the macro.  The
expander continues expanding until the program only uses primitive
``core'' forms.


The following is a toy example of a compile-time function being used
as a macro.
@codeblock|{
#lang racket

(begin-for-syntax
  ;; We can define a compile-time function:
  ;;
  ;; repeat-three: syntax -> syntax
  (define (repeat-three stx)
    (syntax-case stx ()
      [(_ thing)
       (syntax
         (begin thing thing thing))])))
  
;; and we can hook this compile-time function up to the macro expander:
(define-syntax blahblahblah repeat-three)

;; Example:
(blahblahblah (displayln "blah"))
}|


Racket uses an abstract syntax tree structure called a
@link["http://docs.racket-lang.org/reference/syntax-model.html#(tech._syntax._object)"]{syntax
object} to represent programs and tools to manipulate these structured
values. We can pattern-match and pull apart a syntax object with
@racket[syntax-case], and create a new syntax object with
@racket[syntax].  The two forms cooperate with each other: when we
pattern match a syntax-object with @racket[syntax-case], it exposes
the components of the pattern so that they be referenced by
@racket[syntax].


Since it's such a common usage pattern to declare a compile-time
function as a macro, @racket[define-syntax] supports a use that's
analogous to how @racket[define] can be used to define functions.
Racket also includes a small syntax @litchar{#'} that abbreviates
@racket[syntax], in the same way that @litchar{'} abbreviates
@racket[quote].  With these, the above macro can be expressed
succinctly like this:
@codeblock|{
(define-syntax (blahblahblah stx)
  (syntax-case stx ()
    [(_ thing)
     #'(begin thing thing thing)]))
}|



@subsection{Syntax objects are more than s-expressions}

Syntax objects are more than lists and symbols.  They can
hold their source location, which comes in handy if we want to
generate helpful compile-time syntax errors.  For example:
@interaction[#:eval my-eval
@code:comment{Turn on line/column counting for all ports:}
(port-count-lines-enabled #t) 
@code:comment{Read a syntax object:}
(define a-stx (read-syntax #f (open-input-string "(hello this is a test)")))
@code:comment{And inspect it:}
(syntax-case a-stx ()
  [(x y ...)
   (begin
     (displayln  #'x)
     (displayln (syntax-line #'x))
     (displayln (syntax-column #'x))
     (displayln (syntax-position #'x))
     (displayln (syntax-span #'x)))])]


More importantly, they hold @emph{lexical information}, a key element
that allows programs bind and refer to variables.  At the beginning of
compilation, the program's syntax object has little lexical
information.  As the expander walks through the syntax object, though,
it can encounter forms that introduce new bindings.  When the expander
encounters @racket[define], it enriches the lexical information of the
syntax objects in the scope of that binding.

We can even use functions like @racket[identifier-binding] to see this
enrichment taking place.  Let's say that we have a simple definition:
@racketblock[
(define (f x)
  (* x x))
]

We can add do-nothing macros at particular points in this definition
to let us probe what happens during expansion.
@racketblock[
(probe-1
  (define (f x)
    (probe-2
      (* x x))))
]

Let's try this:
@(my-eval '(require (for-syntax racket/base)))
@interaction[#:eval my-eval
(define-syntax (probe-1 stx)
  (syntax-case stx ()
    [(_ (d (f i)
           (p2 (op rand-1 rand-2))))
     (begin 
       (printf "at the first probe: ~a's binding is ~a\n"
               #'rand-1 
               (identifier-binding #'rand-1))
       #'(d (f i)
            (p2 (op rand-1 rand-2))))]))

(define-syntax (probe-2 stx)
  (syntax-case stx ()
    [(_ (op rand-1 rand-2))
     (begin
       (printf "at the second probe: ~a's binding is ~a\n"
               #'rand-1
               (identifier-binding #'rand-1))
       #'(op rand-1 rand-2))]))

@code:comment{Now that we have these probes, let's use it:}
(probe-1
  (define (f x)
    (probe-2
      (* x x))))
]







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
the syntax objects that our macros process.  In
fact, whenever we use @racket[datum->syntax] to take some inert piece
of data and turn it into syntax, the first argument that it takes is a
@emph{source} of lexical information.


Let's amend @racket[def] so that it stores the @racket[stx] object...
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

(require racket/stxparam        ;; syntax parameters are defined in
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

then we end up placing the @racket[splicing-parameterize] accidently
in the scope of the @racket[define].  This wouldn't be so bad, except
for the case that, when Racket processes the @racket[define], it
enriches the syntax objects within the function body with lexical
scoping information for its arguments.

And in particular, it enriches the syntax object that we're intending
to assign to the @racket[current-def] parameter later on.  Oops.  So
we need to take care to keep the @racket[splicing-syntax-parameterize]
outside of the function's body, or else our pristine source of outside
scope will get muddied.




@section{The @racket[outer] limits}

Now that this version of @racket[def] holds on to the currently
expanding definition, other compile-time macros that run in the
context of the body's expansion can access that outside lexical scope.
Let's write @racket[outer] now.  Given something like @racket[(outer
some-id)], we take @racket[some-id], rip the syntaxness out of the
symbol with @racket[syntax-e], and surgically create a new syntax with
the lexical information of the outer scope.
@margin-note{In production code, we'd probably use the @racket[replace-context]
 function from the @racketmodname[syntax/strip-context] library instead.}
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


@section{Beyond the outer limits}

There are a few other things we can do to extend this feature.

   Better error messages with syntax-parse

   (outer <number> id) ...

Making a language that hides define, lambda, block, in favor of our
own scope-saving definitions.


For more information, see ...







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

@section{History}