#lang scribble/manual

@(require scribble/eval
          "scribble-helpers.rkt"
          (for-label racket/base
                     racket/stxparam
                     racket/splicing
                     racket/match))

@(define my-eval (make-base-eval))

@; Theme: SPAAAACE
@;
@; I want to make Portal 2 jokes in here, given the way the
@; outer macro works.  That is essentially what
@; we're setting up: a portal for lexical scope to poke through.
@;
@; Something from Mass Effect would also be nice.
@;
@; As would something from Star Trek.


@inject-javascript|{
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-24146890-1']);
  _gaq.push(['_trackPageview']);
 
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();      
}|




@title{Exploring the boundaries of @tt{outer} space}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


Most programming languages provide a notion of binding and scope.  We
can bind a name somewhere, and then refer to it in some region of
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
in @racket[f] sets up a binding for @racket[x] that blankets the one
from @racket[f]'s.



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


@margin-note{This document should have extensive hyperlinks that reference
into the official documentation.  Many of the words
in @tt{typewriter} font are hyperlinks.
For example, ``@racket[define]'' should be a
hyperlink to the documentation for Racket's binding form.}
The following tutorial shows how we might poke lexical scoping portals
into our programs.  The techniques we'll show here are those that
cooperate with Racket's compiler, so that we won't impose any run-time
penalty.  The tutorial is meant to be self-contained, and expects 
moderate Racket knowledge (functions, modules, structures).


Please let me know if you have any suggestions or comments!




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
can pick up immediately from the on-disk bytecode.


One thing that makes Racket an interesting language is that it allows
its users to hook expressions and functions into the compiler, so that
these compile-time expressions get evaluated and called during the
compilation phase.  And unlike a purely textual pre-processor, these
compile-time expressions can use the full power of Racket.

As a quick example, say that we have the following program:
@filebox["date-at-compile-time.rkt"]{
@codeblock|{
#lang racket
(require (for-syntax racket/date 
                     (planet dyoo/stardate)))
(begin-for-syntax
  (printf "This program is being compiled at Stardate ~a\n" 
          (date->stardate (current-date))))
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
This program is being compiled at Stardate 65741.5
$
}|
}

This output supports the idea that, under normal circumstances,
Racket interposes a compilation phase since it doesn't see any stored
bytecode on disk.

Let's change that.  We'll compile the program, using the @tt{raco make} command:
@nested[#:style 'inset]{
@verbatim|{
$ raco make date-at-compile-time.rkt 
This program is being compiled at Stardate 65741.6
$
}|
}

What is different is that the bytecode has been written to disk, under a
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

@margin-note{For the gory details about Racket's expansion process,
see the
@link["http://docs.racket-lang.org/reference/syntax-model.html"]{reference
manual}.}  One of the main applications of compile-time computation is
to rewrite programs from one form to another.  Racket's compiler has a
built-in @emph{expander} process that uses compile-time functions to
rewrite a program.  Racket's expander is open to extension by letting
us associate a compile-time function to a name; we call such
compile-time functions ``@emph{macros}''.  When the expander sees a
name that's associated to a macro, it applies that macro on a selected
portion of the program and replaces that portion with the value
returned from the macro.  The expander continues expanding until the
program only uses primitive ``core'' forms.


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
  
;; We can hook this compile-time function up to the macro expander:
(define-syntax blahblahblah repeat-three)

;; We can even look at this compile-time binding to blah-blah-blah,
;; by using the syntax-local-value function.
(begin-for-syntax
 (printf "blahblahblah is connected to: ~s\n"
         (syntax-local-value (syntax blahblahblah))))


;; Finally, let's use it.  For example:
(blahblahblah (displayln "blah"))
}|


Racket uses an abstract syntax tree structure called a
@link["http://docs.racket-lang.org/reference/syntax-model.html#(tech._syntax._object)"]{syntax
object} to represent programs.  It provides a variety of tools to manipulate these structured
values. We can pattern-match and pull apart a syntax object with
``@racket[syntax-case]'', and create a new syntax object with
``@racket[syntax]''.

These two forms cooperate with each other: when we pattern match a
syntax-object with @racket[syntax-case], it exposes the components of
the pattern so that they be referenced by @racket[syntax].  In that
sense, @racket[syntax-case] works analogously to @racket[match], but
it is designed to work with the special needs of syntax objects.


Here, we use @racket[define-syntax] to connect a compile-time function
called @racket[repeat-three] to an identifier named
@racket[blahblahblah].  By doing so, we're telling Racket's expander
that whenever it's processing a @racket[blahblahblah], it should use
our @racket[repeat-three] function.  We can even use
@racket[syntax-local-value] at compile-time to see that we've
successfully attached the compile-time function so that the expander
can know about it.


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
@code:comment{Turn on line/column counting for all new ports:}
(port-count-lines-enabled #t) 
@code:comment{Read a syntax object:}
(define a-stx 
  (read-syntax #f (open-input-string 
                   "(Racket is my favorite language on the Citadel)")))
@code:comment{And inspect the individual syntax objects in the structure:}
(for ([piece (syntax->list a-stx)])
  (printf "~a at line ~a, column ~a, position ~a, span ~a\n"
          piece
          (syntax-line piece)
          (syntax-column piece)
          (syntax-position piece)
          (syntax-span piece)))]


More importantly, syntax objects hold @emph{lexical information}, a
key element that allows programs to bind and refer to variables.  At
the beginning of compilation, the program's syntax object has little
lexical information.  As the expander walks through the syntax object,
though, it can encounter forms that introduce new bindings.  When the
expander encounters @racket[define], it enriches the lexical
information of the syntax objects in scope.

We can use functions like @racket[identifier-binding] to see this
enrichment taking place.  Let's say that we have a simple definition:
@racketblock[
(define (cow x)
  (string-append "moooo?" x))
]

We can add do-nothing macros at particular points in this definition
to let us probe what happens during expansion.
@racketblock[
(probe-1
  (define (cow x)
    (probe-2
      (string-append "moooo?" x))))
]

First, let's define the initial @racket[probe-1] macro:
@(my-eval '(require (for-syntax racket/base)))
@interaction[#:eval my-eval
(define-syntax (probe-1 stx)
  (syntax-case stx ()
    [(_ (d (f i)
           (p2 (op rand-1 rand-2))))
     (begin 
       (printf "at probe-1: ~a's binding is ~a\n"
               #'rand-2 
               (identifier-binding #'rand-2))
       #'(d (f i)
            (p2 (op rand-1 rand-2))))]))]

It will tell us what the binding of @racket[x] looks like in the body
of the function; the expander does a top-down walk over the structure
of the syntax object, so @racket[x] shouldn't report any lexical
information at this point.

Now for @racket[probe-2].  The second probe will see what @racket[x]'s
binding looks like after the expander has walked across the
@racket[define]:
@interaction[#:eval my-eval
(define-syntax (probe-2 stx)
  (syntax-case stx ()
    [(_ (op rand-1 rand-2))
     (begin
       (printf "at probe-2: ~a's binding is ~a\n"
               #'rand-2
               (identifier-binding #'rand-2))
       #'(op rand-1 rand-2))]))]

Now that we have these probes, let's use them:
@interaction[#:eval my-eval
(probe-1
  (define (cow x)
    (probe-2
      (string-append "moooo?" x))))
]

As we can see, the expansion process enriches the syntax objects in
the definition of @racket[cow]; @racket[probe-2] shows us that, at the
point where the expander reaches @racket[probe-2], @racket[x] knows it
is lexically bound.

@subsection{Moooo?}

Lexical information isn't just stored in a symbolic syntax object like
@racket[x], but rather it's present in every syntax object.  To
demonstrate this, we can make a @racket[probe-3] that's bit more
disruptive to @racket[cow]: it will take the @racket["moooo?"] out of
the cow and put something else in its place.

We'll use a combination of two tools to perform this surgery:
@racket[datum->syntax] and @racket[with-syntax].
@racket[datum->syntax] lets us create syntax objects with arbitrary
lexical information, and @racket[with-syntax] acts like a @racket[let]
that allows us to inject syntax objects with @racket[syntax].  Just
like @racket[syntax-case], @racket[with-syntax] cooperates with
@racket[syntax] to make it easy to construct new syntaxes.

@interaction[#:eval my-eval
(define-syntax (probe-3 stx)
  (syntax-case stx ()
    [(_ (op rand-1 rand-2))
     (with-syntax ([new-rand-1
                    (datum->syntax #'rand-1 '(string-append x x))])
       #'(op new-rand-1 rand-2))]))

(define (cow x)
  (probe-3
    (string-append "moooo?" x)))

(cow "blah")
]

The use of @racket[datum->syntax] here takes the lexical information
from @racket["moooo?"], and pushes it into a fresh syntax object that
we construct from @racket['(string-append x x)].

And now our @racket[cow] has been transmogrified into
something... familiar, yet unnatural.  How unfortunate.



It's instructive to see what happens if we neglect to preserve the
lexical information when we create syntax objects with
@racket[datum->syntax].  What happens if we just put @racket[#f] in there?

@interaction[#:eval my-eval
(define-syntax (probe-4 stx)
  (syntax-case stx ()
    [(_ (op rand-1 rand-2))
     (with-syntax ([new-rand-1
                    (datum->syntax #f '(string-append x x))])
       #'(op new-rand-1 rand-2))]))

(define (cow x)
  (probe-4
    (string-append "moooo?" x)))]

Poor @racket[cow].  What's important to see is that
@racket['(string-append x x)] has no inherent meaning: it depends on
what we mean by @racket[string-append] and @racket[x], and that is
precisely what lexical information is: it associates meaning to 
meaningless symbols.



Now that we're finished probing @racket[cow], let's go back and see
how to define @racket[outer] in the remaining space we have.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Defining @racket[def]}

We now have a better idea of how macros and lexical scope works.  When
Racket's compiler processes a module, it creates a syntax object out
of the source.  The lexical context of that syntax object initially
holds bindings only from the language of that module; the syntax
object does not yet have any other binding information yet.

What introduces additional binding information is the process of
expansion.  Racket's compiler takes an iterative approach in expanding
the syntax, and when it encounters forms that are meant to bind
variables, such as @racket[define] or @racket[let], then it knows to
enrich the lexical information of the expressions in that binding's
scope.

Now let's break it.

To qualify: we'd like to define an @racket[outer] form that lets us
break lexical scoping in a controlled fashion: we'll allow
@racket[outer] to poke holes along scope boundaries.  Let's say that
the boundaries will be at the outskirts of a function definition.  In
fact, let's make these boundaries explicit, by introducing our own
@racket[def] form.  It will behave similarly to @racket[define].

@codeblock|{
#lang racket

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     #'(define (name args ...) body ...)]))
}|

@racket[def] gives us a function definition syntax.  Let's try it.

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


We want to amend @racket[def] so that it stores the syntax object
representing the function as a whole.  We want this information to be
accessible to other macros that expand when the body of the function
is compiled.  That way, when we're in an @racket[outer], we might take
that stored syntax object and use it as the source of lexical
information in constructing a new syntax, as we did with
@racket[probe-3].



@margin-note{We might use @racket[syntax-parameterize], except that if
we do so, we interfere with how @racket[define] needs to be used in a
@link["http://docs.racket-lang.org/reference/syntax-model.html#(part._expand-context-model)"]{context}
that permits definitions.}
This is a job for
the @racket[splicing-syntax-parameterize] form, which allows us to
maintain this kind of compile-time information during compilation and
share it as we're expanding the body.

@codeblock|{
#lang racket

(require racket/stxparam        ;; syntax parameters are defined in
         racket/splicing)       ;; racket/stxparam and
                                ;; racket/splicing

;; Let's make a compile-time parameter called current-def that
;; remembers the innermost def that's currently being compiled.
(define-syntax-parameter current-def #f)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([fun-stx stx])
       #'(splicing-syntax-parameterize ([current-def #'fun-stx])
           (define (name args ...)
              body ...)))]))
}|



@subsection{The @racket[outer] limits}

Now that this version of @racket[def] holds on to the currently
expanding definition, other compile-time macros that run in the
context of the body's expansion can access that outside lexical scope.
The function @racket[syntax-parameter-value] lets us grab this
information.
We have enough to write @racket[outer] now.  Given something like @racket[(outer
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




@subsection{Timing is everything}
@;
@; Not quite satisfied with this explanation yet.  Need confirmation.
@;
Note the placement of the @racket[splicing-syntax-parameterize] outside the @racket[define]:
this is intentional.  If we do it within,
@codeblock|{
(define-syntax (bad-def stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax ([fun-stx stx])
       #'(define (name args ...)
           (splicing-syntax-parameterize ([current-def #'fun-stx])
             body ...)))]))
}|

then we end up placing the @racket[splicing-parameterize] accidently
in the scope of the @racket[define].  This wouldn't be so bad, except
for the case that, when Racket processes the @racket[define], the
expander enriches the syntax objects within the function body with
lexical scoping information for its arguments.

In particular, it enriches the syntax object that we're intending
to assign to the @racket[current-def] parameter later on.  Ooops.  So
we need to take care to keep the @racket[splicing-syntax-parameterize]
outside of the function's body, or else our pristine source of outside
scope will get muddied.






@(

(lambda () (void))

#|
section{Beyond the outer limits}
@;
@;There are a few other things we can do to extend this feature.
@;
   Better error messages with syntax-parse

   (outer <number> id) ...

Making a language that hides define, lambda, block, in favor of our
own scope-saving definitions.


For more information, see ...

|#

)







@section{Acknowledgements and thanks}

This tutorial arose from
@link["http://lists.racket-lang.org/dev/archive/2012-April/009261.html"]{my
confusion} on how macro expansion works.  Thanks to Robby Findler,
Matthew Flatt, and Ryan Culpepper for helping resolve some my mental
confusion about lexical enrichment.  That being said, I was (am?)
still
@link["http://lists.racket-lang.org/users/archive/2012-April/051357.html"]{confused}.
I want to thank Brian Mastenbrook and Eli Barzilay as well for helping
me out here.  Hopefully this tutorial will help others avoid the same
pitfalls.


@subsection{Revision history}
@itemize[

@item{Early April 2012: Initial release.}

@item{Late April 2012: Changed the @racket[outer] macro so it doesn't
use syntax parameters, based on feedback from Briand Mastenbrook and
Eli Barzilay.  Added some prose to further explain what's going on.}

]