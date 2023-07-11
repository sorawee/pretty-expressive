;; A promise that also records an overestimation of newlines

#lang racket/base

(#%declare #:unsafe)

(provide promise?
         promise-nl
         force
         delay)

(struct promise (nl val) #:mutable)

(define (force v)
  (cond
    [(procedure? (promise-val v))
     (define forced ((promise-val v)))
     (set-promise-val! v forced)
     forced]
    [else (promise-val v)]))

(define-syntax-rule (delay #:nl nl e ...)
  (promise nl (λ () e ...)))
