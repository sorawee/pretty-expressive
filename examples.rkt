#lang racket/base

(require pretty-expressive)

(define d-traditional
  (<> (text "function append(first,second,third){")
      (nest 4
            (let ([f (text "first +")]
                  [s (text "second +")]
                  [t (text "third")])
              (<> nl (text "return ")
                  (group (nest 4 (<> f nl s nl t))))))
      nl (text "}")))

(define d-arbitrary
  (<$> (text "function append(first,second,third){")
       (let ([f (text "first +")]
             [s (text "second +")]
             [t (text "third")]
             [sp (text " ")]
             [indentation (text "    ")]
             [ret (text "return ")])
         (<+> indentation
              (alt (<$> (<+> ret (text "("))
                        (<+> indentation (<$> f s t))
                        (text ")"))
                   (<+> ret f sp s sp t))))
       (text "}")))


(define d-pretty-expressive
  (<> (text "function append(first,second,third){")
      (nest 4
            (let ([f (text "first +")]
                  [s (text "second +")]
                  [t (text "third")])
              (<> nl (text "return ")
                  (alt (<> (text "(")
                           (nest 4 (<> nl f nl s nl t))
                           nl
                           (text ")"))
                       (let ([sp (text " ")])
                         (<> f sp s sp t))))))
      nl
      (text "}")))

(module+ test
  (require rackunit)

  (define horz-layout
    #<<EOF
function append(first,second,third){
    return first + second + third
}
EOF
    )
  (define vert-layout/no-paren
    #<<EOF
function append(first,second,third){
    return first +
        second +
        third
}
EOF
    )
  (define vert-layout/paren
    #<<EOF
function append(first,second,third){
    return (
        first +
        second +
        third
    )
}
EOF
    )
  (check-equal? (pretty-format d-traditional #:page-width 22) vert-layout/no-paren)
  (check-equal? (pretty-format d-arbitrary #:page-width 22) vert-layout/paren)
  (check-equal? (pretty-format d-pretty-expressive #:page-width 22) vert-layout/paren)

  (check-equal? (pretty-format d-traditional #:page-width 36) horz-layout)
  (check-equal? (pretty-format d-arbitrary #:page-width 36) horz-layout)
  (check-equal? (pretty-format d-pretty-expressive #:page-width 36) horz-layout))

(module+ main
  (pretty-print d-traditional #:page-width 22)
  (newline)
  (pretty-print d-arbitrary #:page-width 22)
  (newline)
  (pretty-print d-pretty-expressive #:page-width 22)
  (newline)
  (pretty-print d-traditional #:page-width 36)
  (newline)
  (pretty-print d-arbitrary #:page-width 36)
  (newline)
  (pretty-print d-pretty-expressive #:page-width 36)
  (newline))
