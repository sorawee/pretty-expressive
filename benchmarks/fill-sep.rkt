#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool
         racket/match
         racket/file
         (only-in racket/list take))

(setup "fill-sep" #:size 20000)

(define (fill-sep xs)
  (match xs
    ['() empty-doc]
    [(cons x xs)
     (let loop ([xs xs] [acc (text x)])
       (match xs
         ['() acc]
         [(cons x xs)
          (loop xs (alt (<+> acc space (text x))
                        (<$> acc (text x))))]))]))

(define lines (file->lines (build-path (getenv "BENCHDATA") "words")))
(define doc (fill-sep (take lines (current-size))))
(do-bench doc)
