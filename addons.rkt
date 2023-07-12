;; This module provides more combinators that are built on top of
;; the core combinators

#lang racket/base

(#%declare #:unsafe)

(provide empty-doc
         space
         lparen
         rparen
         lbrack
         rbrack
         lbrace
         rbrace
         comma

         alt

         <>
         <s>
         <$>
         <+>
         <+s>

         flat
         flatten
         group

         define+provide-family)

(require racket/match
         "core.rkt"
         "process.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(define empty-doc (text ""))

(define space (text " "))
(define lparen (text "("))
(define rparen (text ")"))
(define lbrack (text "["))
(define rbrack (text "]"))
(define lbrace (text "{"))
(define rbrace (text "}"))
(define comma (text ","))

(define (alt . xs)
  (for/foldr ([current fail]) ([x (in-list xs)])
    (alternatives x current)))

(define (fold-doc f xs)
  (match xs
    ['() empty-doc]
    [(cons x xs) (for/fold ([current x]) ([x (in-list xs)])
                   (f current x))]))

(define-syntax (define+provide-family stx)
  (syntax-parse stx
    [(_ name:id bin-op:expr)
     #:with name-concat (format-id this-syntax "~a-concat" #'name)
     #:with name-append (format-id this-syntax "~a-append" #'name)
     #'(begin
         (provide name-concat
                  name-append)
         (define (name-concat xs)
           (fold-doc bin-op xs))
         (define name-append
           (case-lambda
             [() empty-doc]
             [(x) x]
             [(x y) (bin-op x y)]
             [xs (name-concat xs)])))]))

(define+provide-family u concat)
(define <> u-append)

(define (us-append/bin x y)
  (<> x space y))
(define+provide-family us us-append/bin)
(define <s> us-append)

(define (v-append/bin x y)
  (<> x nl y))
(define+provide-family v v-append/bin)
(define <$> v-append)

(define (a-append/bin x y)
  (<> x (align y)))
(define+provide-family a a-append/bin)
(define <+> a-append)

(define (as-append/bin x y)
  (<> x space (align y)))
(define+provide-family as as-append/bin)
(define <+s> as-append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flat-map (make-weak-hasheq))

(define (flat d)
  (let loop ([d d])
    (hash-ref! flat-map d
               (λ ()
                 (match d
                   [(struct* :align ([d d])) (doc-process loop d)]
                   [(struct* :nest ([d d])) (doc-process loop d)]
                   [(struct* :nl ()) fail]
                   [_ (doc-process loop d)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flatten-map (make-weak-hasheq))

(define (flatten d)
  (let loop ([d d])
    (hash-ref! flatten-map d
               (λ ()
                 (match d
                   [(struct* :align ([d d])) (doc-process loop d)]
                   [(struct* :nest ([d d])) (doc-process loop d)]
                   [(struct* :nl ()) space]
                   [_ (doc-process loop d)])))))

(define (group d)
  (alt d (flatten d)))

