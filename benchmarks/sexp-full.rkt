#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool)

(setup "sexp-full" #:size 10)

(define (pp s)
  (cond
    [(list? s)
     (define s* (map pp s))
     (<+> (text "(") (alt (v-concat s*) (as-concat s*)) (text ")"))]
    [else (text s)]))

(define (test-expr n c)
  (cond
    [(zero? n) (values (number->string c) (add1 c))]
    [else
     (define-values (t1 c1) (test-expr (sub1 n) c))
     (define-values (t2 c2) (test-expr (sub1 n) c1))
     (values (list t1 t2) c2)]))

(define-values (t _) (test-expr (current-size) 0))
(define doc (pp t))
(do-bench (pretty-format doc))
