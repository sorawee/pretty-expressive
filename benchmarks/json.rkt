#lang racket/base

(module test racket/base)

(require pretty-expressive
         pretty-expressive/benchtool
         racket/match
         racket/file
         (only-in racket/list add-between)
         json)

(setup "json" #:size 2)

(define json-file
  (match (current-size)
    [1 "1k.json"]
    [2 "10k.json"]
    [_ (raise-user-error "invalid size")]))

(define json
  (string->jsexpr (file->string (build-path (getenv "BENCHDATA") json-file))))

(define (h-append/bin a b)
  (<> (flatten a) b))

(define+provide-family h h-append/bin)

(define (enclose-sep left right sep ds)
  (match ds
    ['() (<+> left right)]
    [(list d) (<+> left d right)]
    [(cons d ds)
     (<+> (alt (h-concat (cons left (add-between (cons d ds) sep)))
               (v-concat (cons (<+> left d) (map (λ (d) (<+> sep d)) ds))))
          right)]))

(define (pp v)
  (match v
    [(? list? xs)
     (enclose-sep lbrack rbrack comma (map pp xs))]
    [(? hash? (app hash->list xs))
     (define entries (sort xs symbol<? #:key car))
     (enclose-sep lbrace rbrace comma
                  (map (λ (entry) (<+> (text (format "\"~a\": " (car entry)))
                                       (pp (cdr entry))))
                       entries))]
    [(? number? x) (text (format "~a" (exact->inexact x)))]
    [(? string? x) (text (format "\"~a\"" x))]
    [#f (text "false")]
    [#t (text "true")]))

(define doc (pp json))
(do-bench doc)
