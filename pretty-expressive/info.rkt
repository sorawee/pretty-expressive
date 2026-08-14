#lang info

(define version "1.2")
(define license '(Apache-2.0 OR MIT))

;; ========================================
;; pkg info

(define collection "pretty-expressive")

(define deps
  '("base"
    ["pretty-expressive-lib" #:version "1.2"]))

(define implies
  '("pretty-expressive-lib"))

(define build-deps
  '("racket-doc"
    "scribble-lib"
    "rackunit-lib"))

(define pkg-authors '(sorawee))

(define pkg-desc "A pretty expressive printer")

;; ========================================
;; collect info

(define name "pretty-expressive")
(define scribblings '(("scribblings/pretty-expressive.scrbl" ())))
