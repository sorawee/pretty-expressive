;; The main file for pretty-expressive

#lang racket/base

(require racket/match
         racket/math
         "core.rkt"
         "addons.rkt")

(provide pretty-format/factory/info
         pretty-print/factory/info

         pretty-print/factory
         pretty-format/factory

         pretty-format
         pretty-print

         current-page-width
         current-computation-width
         current-offset
         current-special

         default-cost-factory

         (all-from-out "addons.rkt")
         (except-out (all-from-out "core.rkt")
                     concat ; replaced by <>
                     alternatives ; replaced by alt
                     print-layout))

(define current-page-width (make-parameter 80))
(define current-computation-width (make-parameter #f))
(define current-offset (make-parameter 0))
(define current-special (make-parameter write-special))

(define (pretty-format/factory/info d F
                                   #:offset [offset (current-offset)]
                                   #:special [special (current-special)])
  (define out (open-output-string))
  (define info
    (pretty-print/factory/info d F
                               #:offset offset
                               #:out out
                               #:special special))
  (values (get-output-string out) info))

(define (pretty-print/factory/info d F
                                   #:offset [offset (current-offset)]
                                   #:out [out (current-output-port)]
                                   #:special [special (current-special)])
  (print-layout #:doc d
                #:factory F
                #:offset offset
                #:out out
                #:special special))

(define (pretty-format/factory d F #:offset [offset (current-offset)])
  (define out (open-output-string))
  (define info (pretty-print/factory d F
                                     #:offset offset
                                     #:out out
                                     #:special special))
  (get-output-string out))

(define (pretty-print/factory d F
                              #:offset [offset (current-offset)]
                              #:out [out (current-output-port)]
                              #:special [special (current-special)])
  (void (pretty-print/factory/info d F #:offset offset #:out out #:special special)))

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

(define (pretty-format
         d
         #:page-width [page-width (current-page-width)]
         #:computation-width [computation-width (current-computation-width)]
         #:offset [offset (current-offset)]
         #:special [special (current-special)])
  (define out (open-output-string))
  (pretty-print d
                #:page-width page-width
                #:computation-width computation-width
                #:offset offset
                #:out out
                #:special special)
  (get-output-string out))

(define (pretty-print d
                      #:page-width [page-width (current-page-width)]
                      #:computation-width [computation-width (current-computation-width)]
                      #:offset [offset (current-offset)]
                      #:out [out (current-output-port)]
                      #:special [special (current-special)])
  (pretty-print/factory d
                        (default-cost-factory
                          #:page-width page-width
                          #:computation-width computation-width)
                        #:offset offset
                        #:out out
                        #:special special))
