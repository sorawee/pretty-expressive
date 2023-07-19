;; A facility to process documents

#lang racket/base

(#%declare #:unsafe)

(provide doc-process)
(require racket/match
         "doc.rkt")

(define (doc-process f doc)
  (match doc
    [(struct* :text ()) doc]
    [(struct* :nl ()) nl]
    [(struct* :concat ([a a] [b b]))
     (define a* (f a))
     (define b* (f b))
     (cond
       [(and (eq? a* a) (eq? b* b)) doc]
       [else (concat a* b*)])]
    [(struct* :alternatives ([a a] [b b]))
     (define a* (f a))
     (define b* (f b))
     (cond
       [(and (eq? a* a) (eq? b* b)) doc]
       [else (alternatives a* b*)])]
    [(struct* :align ([d d]))
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (align d*)])]
    [(struct* :nest ([n n] [d d]))
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (nest n d*)])]
    [(struct* :full ([d d]))
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (full d*)])]
    [(struct* :cost ([n n] [d d]))
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (cost n d*)])]
    [(struct* :big-text ()) doc]
    [(struct* :fail ()) fail]))
