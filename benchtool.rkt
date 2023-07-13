;; A benchmarking tool

#lang racket/base

(provide setup
         do-bench
         current-size)

(require racket/cmdline
         racket/match
         racket/string
         racket/format
         file/md5
         pretty-expressive
         pretty-expressive/doc
         (only-in racket/pretty [pretty-write r:pretty-write]))

(define current-size (make-parameter #f))
(define current-out (make-parameter #f))
(define current-program (make-parameter #f))
(define current-view-cost? (make-parameter #f))

(define (setup program
               #:size size
               #:page-width [page-width 80]
               #:computation-width [computation-width 100])
  (current-page-width page-width)
  (current-computation-width computation-width)
  (current-size size)
  (current-out #f)
  (current-program program)
  (current-view-cost? #f)
  (command-line
   #:once-each
   [("--page-width")
    page-width
    [(format "Page width limit (default: ~a)" page-width)]
    (current-page-width (string->number page-width))]
   [("--computation-width")
    computation-width
    [(format "Computation width limit (default: ~a)" computation-width)]
    (current-computation-width (string->number computation-width))]
   [("--size")
    size
    [(format "Size (default: ~a)" size)]
    (current-size (string->number size))]
   [("--view-cost")
    "Output cost (default: no)"
    (current-view-cost? #t)]
   [("--out")
    out
    "Path for the output; - means stdout (default: do not output)"
    (current-out out)]
   [("--memo-limit")
    memo-limit
    "Memoization limit (default: 7)"
    (set-memo-limit! (string->number memo-limit))]))

;; do-bench :: doc? -> void?
(define (do-bench d)
  (match-define-values [(list (info out tainted? cost)) _ duration _]
    (time-apply (λ () (pretty-format/factory/info d (default-cost-factory))) '()))
  (match (current-out)
    [#f (void)]
    ["-" (displayln out)]
    [dest (with-output-to-file dest
            #:exists 'replace
            (λ () (displayln out)))])

  (when (current-view-cost?)
    (fprintf (current-error-port) "(cost ~a)\n" cost))

  (r:pretty-write
   `([target pretty-expressive-racket]
     [program ,(string->symbol (current-program))]
     [duration ,(exact->inexact (/ duration 1000))]
     [lines ,(length (string-split out "\n"))]
     [size ,(current-size)]
     [md5 ,(string->symbol (~a (md5 out)))]
     [page-width ,(current-page-width)]
     [computation-width ,(current-computation-width)]
     [tainted? ,(if tainted? "true" "false")]
     [memo-limit ,(get-memo-limit)])))
