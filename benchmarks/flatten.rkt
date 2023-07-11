#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool)

(setup "flatten" #:size 1000)

(define (quadratic n)
  (cond
    [(zero? n) (text "line")]
    [else (group (<> (quadratic (sub1 n)) nl (text "line")))]))

(define doc (quadratic (current-size)))
(do-bench (pretty-format doc))
