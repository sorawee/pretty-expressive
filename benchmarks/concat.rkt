#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool)

(setup "concat" #:size 10000)

(define (pp n)
  (cond
    [(zero? n) empty-doc]
    [else (<> (pp (sub1 n)) (text "line"))]))

(define doc (pp (current-size)))
(do-bench doc)
