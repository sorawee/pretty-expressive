#lang racket/base

(module+ test
  (require pretty-expressive
           racket/match
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
            (flatten
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

  (check-equal? (pretty-format (nest 4 (reset (<> (text "abc") hard-nl (text "def")))))
                "abc\ndef")
  (check-equal? (pretty-format (nest 4 (<> (text "abc") hard-nl (text "def"))))
                "abc\n    def")
  (check-equal? (pretty-format (alt (flatten (<> (text "abc") nl (text "def")))
                                    (text "something")))
                "abc def")
  (check-equal? (pretty-format (alt (flatten (<> (text "abc") hard-nl (text "def")))
                                    (text "something")))
                "something")

  (check-exn #px"the document fails to print"
             (λ () (pretty-format fail))))