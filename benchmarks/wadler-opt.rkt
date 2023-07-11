#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool)

(setup "wadler-opt" #:size 0 #:page-width 5)

(unless (zero? (current-size))
  (raise-user-error "Size must be zero"))

(define doc
  (<> (group (<> (text "AAA") nl))
      (nest 5
            (group (<> (text "B") nl
                       (text "B") nl
                       (text "B"))))))

(pretty-print doc)
