;; The main file for pretty-expressive

#lang racket/base

(require racket/match
         racket/math
         "core.rkt"
         "addons.rkt")

(provide pretty-format/factory/info

         pretty-print/factory
         pretty-format/factory

         pretty-format
         pretty-print

         current-page-width
         current-computation-width
         current-offset

         default-cost-factory

         (all-from-out "addons.rkt")
         (except-out (all-from-out "core.rkt")
                     concat ; replaced by <>
                     alternatives ; replaced by alt
                     print-layout))

(define current-page-width (make-parameter 80))
(define current-computation-width (make-parameter #f))
(define current-offset (make-parameter 0))

(define (pretty-format/factory/info d F #:offset [offset (current-offset)])
  (print-layout #:doc d
                #:factory F
                #:offset offset))

(define (pretty-format/factory d F #:offset [offset (current-offset)])
  (info-out (pretty-format/factory/info d F #:offset offset)))

(define (pretty-print/factory d F
                              #:offset [offset (current-offset)]
                              #:out [out (current-output-port)])
  (display (pretty-format/factory d F #:offset offset)
           out))

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
         #:offset [offset (current-offset)])
  (pretty-format/factory d
                         (default-cost-factory
                          #:page-width page-width
                          #:computation-width computation-width)
                         #:offset offset))

(define (pretty-print d
                      #:page-width [page-width (current-page-width)]
                      #:computation-width [computation-width (current-computation-width)]
                      #:offset [offset (current-offset)]
                      #:out [out (current-output-port)])
  (display (pretty-format d
                          #:page-width page-width
                          #:computation-width computation-width
                          #:offset offset)
           out))

(module+ test
  (require racket/match
           racket/string
           (except-in rackunit fail))

  (define (get-dim s)
    (define ss (string-split s "\n"))
    (cons (length ss) (apply max (map string-length ss))))

  (define (pretty d)
    (match d
      [(list) (text "()")]
      [(list f args ...)
       (define fp (pretty f))
       (define argsp (map pretty args))
       (alt (<> lparen
                (align (v-concat (cons fp argsp)))
                rparen)
            (<> lparen
                (align fp)
                space
                (align (v-concat argsp))
                rparen)
            (flat
             (<> lparen
                 (align (us-concat (cons fp argsp)))
                 rparen)))]
      [_ (text d)]))

  (define (pretty* d)
    (match d
      [(list) (text "()")]
      [(list f args ...)
       (define fp (pretty* f))
       (define argsp (map pretty* args))
       (alt (<> lparen
                (align (v-concat (cons fp argsp)))
                rparen)
            (<> lparen
                (align fp)
                space
                (align (v-concat argsp))
                rparen)
            (<> lparen
                (align (us-concat (cons fp argsp)))
                rparen))]
      [_ (text d)]))


  (check-equal?
   (pretty-format (pretty '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))
                  #:page-width 31)
   #<<EOF
(+ (foo 1 2)
   (bar 2 3)
   (baz 3 4))
EOF
   )

  (check-equal?
   (get-dim
    (pretty-format (pretty* '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))
                   #:page-width 31))
   (get-dim #<<EOF
(+ (foo 1
        2) (bar 2 3) (baz 3 4))
EOF
            ))

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:page-width 15)
   "(+ 123 456 789)")

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:page-width 14)
   #<<EOF
(+ 123
   456
   789)
EOF
   )

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:page-width 5)
   #<<EOF
(+
 123
 456
 789)
EOF
   )

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:page-width 0)
   #<<EOF
(+
 123
 456
 789)
EOF
   )

  (define abcd '("a" "b" "c" "d"))
  (define abcd4 (list abcd abcd abcd abcd))

  (define p (open-output-string))
  (define prefix "hello: ")
  (display prefix p)
  (pretty-print (pretty (list (list "abcde" abcd4)
                              (list "abcdefgh" abcd4)))
                #:out p
                #:page-width (+ 20 (string-length prefix))
                #:offset (string-length prefix))

  (check-equal? (get-output-string p)
                #<<EOF
hello: ((abcde ((a b c d)
                (a b c d)
                (a b c d)
                (a b c d)))
        (abcdefgh
         ((a b c d)
          (a b c d)
          (a b c d)
          (a b c d))))
EOF
                )

  (check-equal? (pretty-format (nest 4 (big-text "abc\ndef")))
                "abc\ndef")
  (check-equal? (pretty-format (nest 4 (<> (text "abc") nl (text "def"))))
                "abc\n    def")
  (check-equal? (pretty-format (alt (flatten (big-text "a\nb"))
                                    (text "something")))
                "something")
  (check-equal? (pretty-format (alt (flat (big-text "a\nb"))
                                    (text "something")))
                "something"))
