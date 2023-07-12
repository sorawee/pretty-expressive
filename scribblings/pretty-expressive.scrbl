#lang scribble/manual
@require[scribble/example
         scriblib/autobib
         @for-label[pretty-expressive
                    pretty-expressive/process
                    racket/base
                    racket/contract
                    racket/math
                    racket/match]]

@(define evaluator (make-base-eval))
@(evaluator '(require racket/match racket/math pretty-expressive))
@(define-cite ~cite citet generate-bibliography)

@title{pretty-expressive: a pretty expressive printer}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@(define oopsla "Object-Oriented Programming, Systems, Languages and Applications")

@(define Porncharoenwase23:pretty-expressive
   (make-bib #:author (authors "Sorawee Porncharoenwase" "Justin Pombrio" "Emina Torlak")
             #:title @elem{A pretty expressive printer}
             #:location (proceedings-location oopsla)
             #:date "2023"))

@defmodule[pretty-expressive]

This library implements a pretty expressive printer, following the algorithm presented in @citet[Porncharoenwase23:pretty-expressive].
The pretty printer is very expressive, provably optimal, and practically efficient.
It is similar to another library @racketmodname[pprint #:indirect], but that library employs a greedy algorithm.
As a result, @racketmodname[pretty-expressive], when compared to PPrint, is more expressive and optimal, at the cost of being less efficient.

This documentation and its structure are shamelessly copied/adapted from the PPrint library.

@table-of-contents[]

@section{Getting Started}

Pretty printing is a process to produce human-readable text from a structured data.
Users would encode the structured data along with styling choices together as
an abstract document, or simply @deftech{doc}.
Choices in a @tech{doc} could yield many possible layouts.
The pretty printer's task is to choose an optimal layout
(e.g., not exceeding the @deftech{page width limit} while minimizing number of lines)
and present it to users.

Here's a simple example of pretty printing a document encoding a fragment of code.

@examples[#:label #f #:eval evaluator
  (code:comment "Build a document")
  (define doc
    (<> (text "while (true) {")
        (nest 4
              (<> nl
                  (text "f();")
                  nl
                  (<> (text "if (done())")
                      (let ([exit-doc (text "exit();")])
                        (alt (<> space exit-doc)
                             (nest 4 (<> nl exit-doc)))))))
        nl
        (text "}")))
  (code:comment "Print the document (find the optimal layout and render it)")
  (pretty-print doc #:page-width 80)
]

With a different page width limit, the document could be printed differently:

@examples[#:label #f #:eval evaluator
  (pretty-print doc #:page-width 20)
]

@section{Documents}

The library provides many functions (see @secref{Constructing_Documents}) for
building and combining @tech{doc}s, which can then be printed.
(see @secref{Printing_Documents}).

@defproc[(doc? [x any/c]) boolean?]{
  Determines whether @racket[x] is a member of the @tech{doc} datatype.
}

@section{Best Practice for Document Construction}

The arguments to @racket[alt] should roughly have the same content, albeit with different formats.
Although the @deftech{tree size} of a @tech{doc} containing @racket[alt] tends to blow up exponentially,
the time complexity of our algorithm depends on the @deftech{DAG size} of the @tech{doc}.
As a result, provided that sub-documents are sufficiently @emph{shared},
the @tech{DAG size} will be small, allowing efficient pretty printing.

As an example, say we want to pretty print an S-expression with three possible styles for each ``list'': horizontal style, vertical style, and argument list style. That is,

@racketblock[
  (a b c d)
]

could be rendered as itself or

@racketblock[
  (a
   b
   c
   d)
]

or


@racketblock[
  (a b
     c
     d)
]

We can construct a function to convert an S-expression to a @tech{doc}:

@examples[#:eval evaluator #:label #f
  (define (pretty s)
    (match s
      [(list) (<+> lparen rparen)]
      [(list x) (<+> lparen (pretty x) rparen)]
      [(list x xs ...)
       (code:comment @#,elem{Calculate all subdocuments first to @emph{share} their references})
       (define x-doc (pretty x))
       (define xs-doc (map pretty xs))
       (<+> lparen
            (alt (as-concat (cons x-doc xs-doc))
                 (v-concat (cons x-doc xs-doc))
                 (<+> x-doc space (v-concat xs-doc)))
            rparen)]
      [_ (text s)]))
]

We can then pretty print it:

@examples[#:eval evaluator #:label #f
  (define abcd-doc (pretty '("a" "b" "c" "d")))
  (pretty-print abcd-doc #:page-width 10)
  (pretty-print abcd-doc #:page-width 6)
  (pretty-print abcd-doc #:page-width 4)
]

The important point is that we @emph{reuse} @racket[x-doc] and @racket[xs-doc] across branches of @racket[alt].
Had we call @racket[(pretty x)] and @racket[(map pretty xs)] multiple times in branches of @racket[alt],
both @tech{doc} construction and @racket[pretty-print] would be inefficient.

@section{Library Documentation}

@subsection{Printing Documents}

@defproc[(pretty-print [d doc?]
                       [#:page-width page-width natural? (current-page-width)]
                       [#:computation-width computation-width (or/c #f natural?) (current-computation-width)]
                       [#:offset offset natural? (current-offset)]
                       [#:out out output-port? (current-output-port)])
         void?]{
  Pretty prints the @tech{doc} @racket[d] to the output port @racket[out]
  with a maximum page width of @racket[page-width] and offset @racket[offset].
  The optimality of the output is only guanranteed when the output fits the @deftech{computation width} @racket[computation-width].
  The worst case time complexity of pretty printing is proportional to the DAG size of @racket[d] and
  the 4th power of @racket[computation-width] (although in practice it is much lower than that).
  If @racket[computation-width] has the @racket[#f] value, its effective value is @math{1.2 × @racket[page-width]}.

  The optimality objective for this pretty printing is according to @racket[default-cost-factory].

@examples[#:eval evaluator
  (define prefix-s "values are: ")
  (define doc (<$> (<+> lparen
                        (<$> (text "'Rhoam Bosphoramus Hyrule'")
                             (text "'Daphnes Nohansen Hyrule'"))
                        rparen)
                   (<+> lparen
                        (text "'2B'")
                        space
                        (text "'9S'")
                        space
                        (text "'A2'")
                        rparen)))
  (begin
    (display prefix-s)
    (pretty-print doc #:offset (string-length prefix-s)))
  (begin
    (display prefix-s)
    (pretty-print doc))]}

@defproc[(pretty-format [d doc?]
                        [#:page-width page-width natural? (current-page-width)]
                        [#:computation-width computation-width (or/c #f natural?) (current-computation-width)]
                        [#:offset offset natural? (current-offset)])
         string?]{
  Like @racket[pretty-print], but outputs a string instead.
}


@defproc[(pretty-print/factory [d doc?]
                               [F cost-factory?]
                               [#:offset offset natural? (current-offset)]
                               [#:out out output-port? (current-output-port)])
         void?]{
  Like @racket[pretty-print], but uses a cost factory @racket[F] instead.
  See @secref{Cost_factory} for more details.
}

@defproc[(pretty-format/factory [d doc?]
                                [F cost-factory?]
                                [#:offset offset natural? (current-offset)])
         string?]{
  Like @racket[pretty-print/factory], but outputs a string instead.
}

@defproc[(pretty-format/factory/info [d doc?]
                                     [F cost-factory?]
                                     [#:offset offset natural? (current-offset)])
         info?]{
  Like @racket[pretty-print/factory], but outputs an @racket[info] structure
  which contains debugging information.
}

@defstruct[info ([out string?]
                 [tainted? boolean?]
                 [cost any/c])]{
  A structure type that contains both the output of pretty printing and debugging information:
  taintedness and cost of the output layout.
}

@subsection{Constructing Documents}

@defproc[(text [s string?]) doc?]{
  Constructs a @tech{doc} containing the fixed string @racket[s].
  @racket[s] must @bold{not} contain a newline character.
}

@defthing[nl doc?]{
  A newline document.
}

@defproc[(alt [x doc?] ...) doc?]{
  Constructs a @tech{doc} which is rendered to one of @racket[x]s, whichever resulting in the prettiest layout for the whole document. If given no arguments, the resulting doc is @racket[fail].

  See also @secref["Best_Practice_for_Document_Construction"].
}

@deftogether[(@defproc[(v-append [x doc?] ...) doc?]
              @defproc[(<$> [x doc?] ...) doc?])]{
  Concatenates @tech{doc} @racket[x]s vertically.

  @examples[#:eval evaluator
    (pretty-print
      (<$> (text "Splatoon 2")
           (text "Nier Automata")
           (text "Breath of the Wild")))
  ]
}

@defproc[(v-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] vertically.
}

@deftogether[(@defproc[(u-append [x doc?] ...) doc?]
              @defproc[(<> [x doc?] ...) doc?])]{
  Concatenates @tech{doc} @racket[x]s together without alignment.

  @examples[#:eval evaluator
  (define left-doc
    (<$> (text "Splatoon 2")
         (text "Nier")))
  (define right-doc
    (<$> (text "Automata")
         (text "Breath of the Wild")))
    (pretty-print (<> left-doc right-doc))
  ]
}

@deftogether[(@defproc[(a-append [x doc?] ...) doc?]
              @defproc[(<+> [x doc?] ...) doc?])]{
  Concatenates @tech{doc} @racket[x]s together with alignment.

  @examples[#:eval evaluator
    (pretty-print (<+> left-doc right-doc))
  ]
}

@deftogether[(@defproc[(us-append [x doc?] ...) doc?]
              @defproc[(<s> [x doc?] ...) doc?])]{
  Concatenates @tech{doc} @racket[x]s together without alignment
  with successive pairs separated by @racket[space].

  @examples[#:eval evaluator
    (pretty-print (<s> left-doc right-doc))
  ]
}

@deftogether[(@defproc[(as-append [x doc?] ...) doc?]
              @defproc[(<+s> [x doc?] ...) doc?])]{
  Concatenates @tech{doc} @racket[x]s together with alignment
  with successive pairs separated by @racket[space].

  @examples[#:eval evaluator
    (pretty-print (<+s> left-doc right-doc))
  ]
}

@defproc[(u-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] together using @racket[<>].
}

@defproc[(a-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] together using @racket[<+>].
}

@defproc[(us-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] together using @racket[<s>].
}

@defproc[(as-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] together using @racket[<+s>].
}

@defproc[(sep [xs doc?]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] either (1) vertically or (2)
  horizontally with successive pairs separated by @racket[space].
}

@defproc[(align [d doc?]) doc?]{
  Aligns the @tech{doc} @racket[d].
  @racket[(<+> a b)] is equivalent to @racket[(<> a (align b))].
}

@defproc[(nest [n natural?] [d doc?]) doc?]{
  Increments the indentation level by @racket[n] when rendering the @tech{doc} @racket[d].
}

@defthing[fail doc?]{
  Constructs a @tech{doc} that fails to render.
  This doc interacts with @racket[alt]: only non-failing branches are considered for rendering.
}

@defproc[(flat [x doc?]) doc?]{
  Constrains @tech{doc} @racket[x] to fit in one line.
  If @racket[x] can't fit in one line, it fails to render.
}

@defproc[(flatten [x doc?]) doc?]{
  Flattens @tech{doc} @racket[x] so that all newlines are replaced with spaces.
}

@defproc[(group [x doc?]) doc?]{
  Creates two choices: @racket[(flatten x)] and @racket[x].
}

@defproc[(cost [n any/c] [x doc?]) doc?]{
  Adds a cost @racket[n] to @racket[x].
}

@subsection{Useful Constants}


@defthing[empty-doc doc?]{
  Same as @racket[(text "")]
}

@defthing[lparen doc?]{
  Same as @racket[(text "(")]
}

@defthing[rparen doc?]{
  Same as @racket[(text ")")]
}

@defthing[lbrack doc?]{
  Same as @racket[(text "[")]
}

@defthing[rbrack doc?]{
  Same as @racket[(text "]")]
}

@defthing[lbrace doc?]{
  Same as @racket[(text "{")]
}

@defthing[rbrace doc?]{
  Same as @racket[(text "}")]
}

@defthing[space doc?]{
  Same as @racket[(text " ")]
}

@defthing[comma doc?]{
  Same as @racket[(text ",")]
}

@subsection{Parameters}

@defparam[current-page-width page-width natural? #:value 80]{
  A parameter that determines the page width.
}

@defparam[current-computation-width computation-width (or/c #f natural?) #:value #f]{
  A parameter that determines the @tech{computation width}.
}

@defparam[current-offset offset natural? #:value 0]{
  A parameter that determines the column offset for subsequent lines.
}

@subsection{Match Expanders}

Internally, a @tech{doc} is either a @racket[:text], @racket[:nl], @racket[:concat], @racket[:alternatives], @racket[:align], @racket[:nest], @racket[:full], @racket[:fail], or @racket[:cost].
We provide these @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander}s to allow @tech{doc} processing (see @secref{Processing_Documents}.
The match expanders are illegal outside of the pattern position of the @racket[match] form.
Keep in mind that this list is unstable and could change across versions of the library.


@defform[(:text s)]{
  A match expander that recognizes text @racket[s] of type @racket[string?].
}

@defform[(:nl)]{
  A match expander that recognizes a newline.
}

@defform[(:concat da db)]{
  A match expander that recognizes an unaligned concatenation of @tech{doc}s @racket[da] and @racket[db].
}

@defform[(:alternatives da db)]{
  A match expander that recognizes a choice: @tech{doc}s @racket[da] and @racket[db].
}

@defform[(:align d)]{
  A match expander that recognizes an alignment of @tech{doc} @racket[d].
}

@defform[(:nest n d)]{
  A match expander that recognizes an increment of indentation level of @racket[n] on @tech{doc} @racket[d].
}

@defform[(:full d)]{
  A match expander that recognizes a constraint that the @tech{doc} @racket[d] must not be followed by any any non-empty text in the line.
}

@defform[(:fail)]{
  A match expander that recognizes a failing @tech{doc}.
}

@defform[(:cost c d)]{
  A match expander that recognizes an increment of cost by @racket[c] on the @tech{doc} @racket[d].
}

@section{Cost factory}

Pretty printers choose an optimal layout from a document
by minimizing an @deftech{optimality objective}.
Unlike other pretty printers, which have built-in optimality objectives,
@racketmodname[pretty-expressive] allows users to customize an optimality objective via
the @deftech{cost factory} interface.

@defstruct[cost-factory ([cost<=? (-> any/c any/c any/c)]
                         [cost+ (-> any/c any/c any/c)]
                         [cost-text (-> natural? natural? any/c)]
                         [cost-nl (-> natural? any/c)]
                         [limit natural?])]{
  A structure type for cost factories.

  @itemlist[
    @item{@racket[(cost<=? a b)] determines whether the cost @racket[a] is less than or equal to the cost @racket[b].}
    @item{@racket[(cost+ a b)] combines costs @racket[a] and @racket[b] together to produce a new cost.}
    @item{@racket[(cost-text c len)] produces a cost due to a text placement at column position @racket[c] when the text has length @racket[len].}
    @item{@racket[(cost-nl i)] produces a cost due to a newline along with @racket[i] indentation spaces.}
    @item{@racket[limit] is the actual value for computation width limit.}
  ]

  These functions should at minimum satisfy the following properties:

  @itemlist[
    @item{@racket[cost<=?] should be a total order: reflexive, antisymmetric, and total.}
    @item{For all costs @racket[a], @racket[b], @racket[c], and @racket[d], such that @racket[(cost<=? a b)] and @racket[(cost<=? c d)],
          @racket[cost+] should satisfy @racket[(cost<=? (cost+ a c) (cost+ b d))].}
    @item{For all @racket[c], @racket[c*], and @racket[i], such that @racket[(<= c c*)],
         @racket[cost-text] should satisfy @racket[(cost<=? (cost-text c i) (cost-text c* i))].}
    @item{For all @racket[i] and @racket[i*] such that @racket[(<= i i*)],
          @racket[cost-nl] should satisfy @racket[(cost<=? (cost-nl i) (cost-nl i*))].}
  ]

  But for a cost factory to be well-formed in presence of rewriting rules, further properties are required:

  @itemlist[
    @item{@racket[cost+] should be commutative and associative.}
    @item{@racket[(cost+ (cost-text c len) (cost-text (+ c len) len*))] should be equal to @racket[(cost-text c (+ len len*))].}
  ]
}

@defproc[(default-cost-factory [#:page-width page-width natural? (current-page-width)]
                               [#:computation-width computation-width (or/c #f natural?) (current-computation-width)])
         cost-factory?]{
  The default cost factory that is employed for @racket[pretty-print].
  A cost satisfies the contract @racket[(list/c natural? natural?)].
  For a cost @racket[(list b h)], @racket[b] is the @deftech{badness}, which is the sum of squared overflows,
  and @racket[h] is the number of newlines.
  The optimality objective is to minimize the badness, and then minimize the number of newlines.

  Internally, this cost factory is implemented as:

  @racketblock[
    (define (default-cost-factory
             #:page-width [page-width (current-page-width)]
             #:computation-width [computation-width (current-computation-width)])
      (cost-factory
       (match-lambda**
        [((list b1 h1) (list b2 h2))
         (cond
           [(= b1 b2) (<= h1 h2)]
           [else (< b1 b2)])])
       (match-lambda**
        [((list b1 h1) (list b2 h2))
         (list (+ b1 b2) (+ h1 h2))])
       (λ (pos len)
         (define stop (+ pos len))
         (cond
           [(> stop page-width)
            (define maxwc (max page-width pos))
            (define a (- maxwc page-width))
            (define b (- stop maxwc))
            (list (* b (+ (* 2 a) b)) 0)]
           [else (list 0 0)]))
       (λ (i) (list 0 1))
       (or computation-width (exact-floor (* page-width 1.2)))))
  ]
}

@subsection{More cost factory examples}

Consider the example in @secref{Best_Practice_for_Document_Construction}.
Each list can be rendered with three possible styles: horizontal style, vertical style, and argument list style.

@examples[#:eval evaluator #:label #f
  (pretty-print (pretty '("abc" "def" ("ghi" "jkl" "mno"))) #:page-width 15)
]

Indeed, this is an optimal layout according to @racket[default-cost-factory],
because it does not have any @tech{badness}, and two newlines are minimal.

However, let's say that we consider the vertical style to be not pretty.
The vertical style should still be a possibility however, since it can help us avoid going over the page width limit
and minimize the number of newlines in many situations.
We simply would prefer other styles when all else is equal.
In this case, we would prefer the output:

@racketblock[
  (abc def
       (ghi jkl
            mno))
]

To address this issue, we construct a new cost factory.

  @examples[#:label #f #:eval evaluator
    (define (my-cost-factory
             #:page-width [page-width (current-page-width)]
             #:computation-width [computation-width (current-computation-width)])
      (cost-factory
       (match-lambda**
        [((list b1 h1 sc1) (list b2 h2 sc2))
         (cond
           [(= b1 b2)
            (cond
              [(= h1 h2) (<= sc1 sc2)]
              [else (< h1 h2)])]
           [else (< b1 b2)])])
       (match-lambda**
        [((list b1 h1 sc1) (list b2 h2 sc2))
         (list (+ b1 b2) (+ h1 h2) (+ sc1 sc2))])
       (λ (pos len)
         (define stop (+ pos len))
         (cond
           [(> stop page-width)
            (define maxwc (max page-width pos))
            (define a (- maxwc page-width))
            (define b (- stop maxwc))
            (list (* b (+ (* 2 a) b)) 0 0)]
           [else (list 0 0 0)]))
       (λ (i) (list 0 1 0))
       (or computation-width (exact-floor (* page-width 1.2)))))
  ]

The cost of @racket[my-cost-factory] is similar to that of @racket[default-cost-factory],
but it has an extra component: @deftech{style cost}.
When all else is equal, we prefer a cost with less style cost.

We can now construct a function to convert an S-expression to a @tech{doc}.
It penalizes the vertical style by adding a @tech{style cost} to that choice.

@examples[#:eval evaluator #:label #f
  (define (new-pretty s)
    (match s
      [(list) (<+> lparen rparen)]
      [(list x) (<+> lparen (new-pretty x) rparen)]
      [(list x xs ...)
       (define x-doc (new-pretty x))
       (define xs-doc (map new-pretty xs))
       (<+> lparen
            (alt (as-concat (cons x-doc xs-doc))
                 (code:comment "Add a style cost to penalize the vertical style")
                 (cost (list 0 0 1) (v-concat (cons x-doc xs-doc)))
                 (<+> x-doc space (v-concat xs-doc)))
            rparen)]
      [_ (text s)]))
]

Now we can pretty print as we desired:

@examples[#:eval evaluator #:label #f
  (pretty-print/factory (new-pretty '("abc" "def" ("ghi" "jkl" "mno")))
                        (my-cost-factory #:page-width 15))

  (code:comment "Three styles are still possible")
  (define new-abcd-doc (new-pretty '("a" "b" "c" "d")))
  (pretty-print/factory new-abcd-doc (my-cost-factory #:page-width 10))
  (pretty-print/factory new-abcd-doc (my-cost-factory #:page-width 6))
  (pretty-print/factory new-abcd-doc (my-cost-factory #:page-width 4))
]

@section{Processing Documents}

@defmodule[pretty-expressive/process]

@defproc[(doc-process [f procedure?] [d doc?]) doc?]{
  Calls @racket[f] on the immediate subdocuments of @racket[d] and reassembles the results back.
  The function attempts to avoid creating new objects as best as it can. Note that @racket[f] should be memoized.

  Prefer using this function over manual @racket[match]ing against all match expanders.
  The list of match expanders could change across versions of this library,
  making programs that directly matches against all expanders brittle to changes.
  Using this function on the other hand makes doc processing stable across versions.
}

@(generate-bibliography)
