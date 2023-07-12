;; This module defines basic document combinator along with
;; the basic partial evaluation.

#lang racket/base

(#%declare #:unsafe)

(provide doc?

         ;; pattern expanders
         :nl
         :fail
         :text
         :alternatives
         :concat
         :nest
         :align
         :full
         :cost

         ;; constructor
         nl
         fail
         text
         alternatives
         concat
         nest
         align
         full
         cost

         set-memo-limit!
         get-memo-limit)

(module+ private
  (provide (struct-out doc)
           make-text))

(require racket/match
         (for-syntax racket/base
                     syntax/parse/pre))

;; We only memoize every memo-weight-limit node.
;; The value of memo-weight-limit must be a positive integer (initialized below)
(define memo-weight-limit #f)

;; memo-weight-init is an integer in range [0, *memo-weight-limit* - 1]
;; that is used as the initial value given to the leaf nodes.
;; Since we don't want the leaf node to get memoized and want memoization to
;; occur farthest away from the leaf nodes,
;; we set the value to memo-weight-limit - 1 (initialized below).
(define memo-weight-init #f)

(define (set-memo-limit! memo-limit)
  (set! memo-weight-limit memo-limit)
  (set! memo-weight-init (sub1 memo-limit)))

(define (get-memo-limit)
  memo-weight-limit)

(set-memo-limit! 7)

;; - failing/X/Y means the doc will always fail to resolve.
;;   X indicates fullness before resolving, and
;;   Y indicates fullness after resolving.
;;   The value usually starts as #f. The #f value could be mutated to #t.
(struct doc (memo-weight ; memo weight in range of 0 to *memo-weight-limit* - 1
             table/no/no ; memo table for not full before and not full after
             table/yes/no ; memo table for full before and not full after
             table/no/yes ; memo table for not full before and full after
             table/yes/yes ; memo table for full before and full after
             failing/no/no ; fails when not full before and not full after?
             failing/yes/no ; fails when full before and not full after?
             failing/no/yes ; fails when not full before and full after?
             failing/yes/yes ; fails when full before and full after?
             nl-cnt) ; overapproximation of newline count
  #:transparent
  #:mutable)

(begin-for-syntax
  (define-splicing-syntax-class fail-flag
    (pattern {~seq #:fail value ...})
    (pattern {~seq}
             #:with (value ...) #'(#f #f #f #f))))

;; calc-weight :: doc -> natural?
;; Calculate the current memo weight in range 0 to memo-weight-limit - 1
(define (calc-weight d)
  (define val (doc-memo-weight d))
  (cond
    [(zero? val) memo-weight-init]
    [else (sub1 val)]))

(define-syntax (get-weight stx)
  (syntax-parse stx
    [(_ d) #'(calc-weight d)]
    [(_ a b) #'(min (calc-weight a) (calc-weight b))]))

;; inst-internal-doc creates an internal doc with a constructor ctor.
;; It automatically decides whether memo tables should be allocated based on
;; the memo weight, and initializes the failure flags with #f if
;; #:fail is not given
(define-syntax (inst-internal-doc stx)
  (syntax-parse stx
    [(_ ctor #:doc [doc ...]
        fail:fail-flag #:args arg ...)
     #'(let ([weight (get-weight doc ...)])
         (cond
           [(zero? weight)
            (ctor weight
                  (make-hasheq) (make-hasheq) (make-hasheq) (make-hasheq)
                  fail.value ...
                  arg ...)]
           [else
            (ctor weight
                  #f #f #f #f
                  fail.value ...
                  arg ...)]))]))

;; inst-leaf-doc creates a leaf doc with a constructor ctor.
;; It automatically blanks the memo table fields, initializes the memo weight
;; and initializes the failure flags with #f if #:fail is not given
(define-syntax (inst-leaf-doc stx)
  (syntax-parse stx
    [(_ ctor fail:fail-flag #:args arg ...)
     #'(ctor memo-weight-init
             #f #f #f #f
             fail.value ...
             arg ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A nl is a nullary constructor
;; (a potential extension could allow it to contain a string for replacement
;; under flatten)
(struct :nl doc () #:transparent #:constructor-name make-nl)

(define nl (inst-leaf-doc make-nl #:fail #f #f #t #t #:args 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A fail is a nullary constructor.
(struct :fail doc () #:transparent #:constructor-name make-fail)

(define fail (inst-leaf-doc make-fail #:fail #t #t #t #t #:args -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A text consists of:
;; - s :: (or/c string? (treeof string?))
;; - len :: natural?
;; where len is the length of s
(struct :text doc (s len) #:transparent #:constructor-name make-text*)

(define (make-text s len)
  (inst-leaf-doc make-text* #:fail #f #t #t (not (zero? len)) #:args 0 s len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An alternatives consists of:
;; - a :: doc?
;; - b :: doc?
(struct :alternatives doc (a b) #:transparent #:constructor-name make-alternatives*)

(define (make-alternatives a b)
  (inst-internal-doc make-alternatives*
                     #:doc [a b]
                     #:args
                     (max (doc-nl-cnt a) (doc-nl-cnt b))
                     a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A concat consists of:
;; - a :: doc?
;; - b :: doc?
(struct :concat doc (a b) #:transparent #:constructor-name make-concat*)

(define (make-concat a b)
  (inst-internal-doc make-concat*
                     #:doc [a b]
                     #:args
                     (+ (doc-nl-cnt a) (doc-nl-cnt b))
                     a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A nest consists of:
;; - n :: natural?
;; - d :: doc?
(struct :nest doc (n d) #:transparent #:constructor-name make-nest*)

(define (make-nest n d)
  (inst-internal-doc make-nest*
                     #:doc [d]
                     #:args
                     (doc-nl-cnt d)
                     n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An align consists of:
;; - d :: doc?
(struct :align doc (d) #:transparent #:constructor-name make-align*)

(define (make-align d)
  (inst-internal-doc make-align*
                     #:doc [d]
                     #:args
                     (doc-nl-cnt d)
                     d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A full consists of:
;; - d :: doc?
(struct :full doc (d) #:transparent #:constructor-name make-full*)

(define (make-full d)
  (inst-internal-doc make-full*
                     #:doc [d]
                     #:fail #t #t #f #f
                     #:args
                     (doc-nl-cnt d)
                     d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A cost consists of:
;; - n :: tau
;; - d :: doc?
(struct :cost doc (n d) #:transparent #:constructor-name make-cost*)

(define (make-cost n d)
  (inst-internal-doc make-cost*
                     #:doc [d]
                     #:args
                     (doc-nl-cnt d)
                     n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set current-debug? to true for the debug mode (disable partial evaluation)
(define-for-syntax current-debug? #f)

;; perform partial evaluation on the "constructor"s

;; For production code, use #:prod clause. For debugging code, use #:dbg clause.
(define-syntax (cond-dbg stx)
  (syntax-parse stx
    [(_ [#:dbg e ...+]
        [#:prod e2 ...+])
     #:with out (if current-debug?
                    #'(let () e ...)
                    #'(let () e2 ...))
     #'out]))

(cond-dbg
 [#:dbg
  (displayln "==========")
  (displayln "debug mode")
  (displayln "==========")]
 [#:prod (void)])

(define (text s)
  (make-text s (string-length s)))

(define (concat a b)
  (cond-dbg
   [#:dbg (make-concat a b)]
   [#:prod (match* (a b)
             [((struct* :text ([len 0])) d) d]
             [(d (struct* :text ([len 0]))) d]
             [((? :full?) (? :text?)) fail] ; the text is non-empty
             [((? :fail?) _) fail]
             [(_ (? :fail?)) fail]
             [((struct* :text ([s sa] [len la]))
               (struct* :text ([s sb] [len lb])))
              (make-text (cons sa sb) (+ la lb))]
             [(_ _) (make-concat a b)])]))

(define (alternatives a b)
  (cond-dbg
   [#:dbg (make-alternatives a b)]
   [#:prod (match* (a b)
             [((? :fail?) _) b]
             [(_ (? :fail?)) a]
             [(_ _)
              (cond
                [(eq? a b) a]
                [else (make-alternatives a b)])])]))

(define (full d)
  (cond-dbg
   [#:dbg (make-full d)]
   [#:prod (match d
             [(? :full?) d]
             [(? :fail?) fail]
             [_ (make-full d)])]))

(define (cost n d)
  (cond-dbg
   [#:dbg (make-cost n d)]
   [#:prod (match d
             [(? :fail?) fail]
             [_ (make-cost n d)])]))

(define (nest n d)
  (cond-dbg
   [#:dbg (make-nest n d)]
   [#:prod (match d
             [(? :fail?) fail]
             [(? :align?) d]
             [(? :text?) d]
             [(struct* :nest ([n n2] [d d])) (make-nest (+ n n2) d)]
             [_ (make-nest n d)])]))

(define (align d)
  (cond-dbg
   [#:dbg (make-align d)]
   [#:prod (match d
             [(? :fail?) fail]
             [(? :align?) d]
             [(? :text?) d]
             [_ (make-align d)])]))
