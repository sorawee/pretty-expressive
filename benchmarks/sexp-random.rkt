#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool
         racket/file
         json)

(setup "sexp-random" #:size 1)

(define (pp s)
  (cond
    [(list? s)
     (define s* (map pp s))
     (<+> (text "(") (alt (v-concat s*) (as-concat s*)) (text ")"))]
    [else (text s)]))

(define json
  (string->jsexpr
   (file->string
    (build-path (getenv "BENCHDATA")
                (format "random-tree-~a.sexp" (current-size))))))

(define doc (pp json))
(do-bench (pretty-format doc))
