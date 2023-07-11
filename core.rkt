;; This module is the core of the pretty expressive printer algorithm

#lang racket/base

(#%declare #:unsafe)

(define-logger pretty-expressive)

(provide print-layout
         (struct-out cost-factory)
         (all-from-out "doc.rkt"))

(require racket/match
         racket/list
         racket/string
         "doc.rkt"
         "process.rkt"
         "promise.rkt"
         (submod "doc.rkt" private))

(struct cost-factory (cost<=? cost+ cost-text cost-nl limit))

;; A measure consists of
;; - last length :: natural?
;; - cost :: tau
;; - token function :: (listof string) -> (listof string?)
(struct measure (last cost tok) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; measure-set/c is either
;; - a promise that can be forced to a list of measures with length at most one
;; - a list of measures

;; extract-at-most-one :: measure-set/c -> (or/c null? (list/c measure?))
(define (extract-at-most-one ms)
  (match ms
    [(? promise?) (force ms)]
    ['() '()]
    [(list m _ ...) (list m)]))

;; NOTE: the token function takes a list of tokens and produces a list of tokens.
;; We could alternatively make it consume nothing and
;; use side-effect to write content to a port directly.
;; Unfortunately, due to https://github.com/racket/racket/issues/1388
;; the latter is currently inefficient, so we will build a list of tokens
;; explicitly for now.

;; print :: doc -> cost-factory? -> measure?
(define (print d F #:offset offset)
  (match-define (cost-factory cost<=? cost+ cost-text cost-nl limit) F)

  (define (dominates? m1 m2)
    (match-define (measure last1 cost1 _) m1)
    (match-define (measure last2 cost2 _) m2)
    (and (<= last1 last2) (cost<=? cost1 cost2)))

  ;; merge :: measure-set/c -> measure-set/c -> #:keep-both? boolean?
  ;;          -> measure-set/c
  ;; Merge two measure sets together. #:prunnable? should be #f when there is a
  ;; chance that the two measure sets are both tainted,
  ;; and when they are forced, one fails while one does not.
  (define (merge ms1 ms2 #:prunable? [prunable? #f])
    (match* (ms1 ms2)
      [(_ '()) ms1]
      [('() _) ms2]
      [((? promise?) (? promise?))
       (let-values ([(ms1 ms2)
                     (cond
                       [(>= (promise-nl ms1) (promise-nl ms2)) (values ms1 ms2)]
                       [else (values ms2 ms1)])])
         (if prunable?
             ms1
             (delay #:nl (promise-nl ms1)
                    (match (force ms1)
                      ['() (force ms2)]
                      [val val]))))]
      [(_ (? promise?)) ms1]
      [((? promise?) _) ms2]
      [(_ _)
       (let loop ([ms1 ms1] [ms2 ms2])
         (match* (ms1 ms2)
           [(_ '()) ms1]
           [('() _) ms2]
           [((cons m1 ms1*) (cons m2 ms2*))
            (cond
              [(dominates? m1 m2) (loop ms1 ms2*)]
              [(dominates? m2 m1) (loop ms1* ms2)]
              [(> (measure-last m1) (measure-last m2)) (cons m1 (loop ms1* ms2))]
              [else (cons m2 (loop ms1 ms2*))])]))]))

  (define (concat-measure m1 m2)
    (match-define (measure _ cost1 tok1) m1)
    (match-define (measure last2 cost2 tok2) m2)
    (measure last2 (cost+ cost1 cost2) (λ (tks) (tok1 (tok2 tks)))))

  (define limit+1 (add1 limit))

  (define (memoize f)
    (λ (d c i beg-full? end-full?)
      (cond
        [((if beg-full?
              (if end-full? doc-failing/yes/yes doc-failing/yes/no)
              (if end-full? doc-failing/no/yes doc-failing/no/no)) d)
         '()]
        [(or (> c limit) (> i limit) (not (zero? (doc-memo-weight d))))
         (f d c i beg-full? end-full?)]
        [else
         (define table ((if beg-full?
                            (if end-full? doc-table/yes/yes doc-table/yes/no)
                            (if end-full? doc-table/no/yes doc-table/no/no)) d))
         (hash-ref! table
                    (+ (* i limit+1) c)
                    (λ () (f d c i beg-full? end-full?)))])))

  (define resolve
    (memoize
     (λ (d c i beg-full? end-full?)
       (define (core)
         (match d
           [(struct* :text ([s s] [len len]))
            (define (m)
              (list (measure (+ c len)
                             (cost-text c len)
                             (λ (tks)
                               (cond
                                 [(string? s) (cons s tks)]
                                 [else (append (flatten s) tks)])))))
            (cond
              [beg-full?
               (cond
                 [(and end-full? (zero? len)) (m)]
                 [else '()])]
              [else
               (cond
                 [end-full? '()]
                 [else (m)])])]

           [(struct* :full ([d d]))
            (cond
              [end-full?
               (merge (resolve d c i beg-full? #f) (resolve d c i beg-full? #t))]
              [else '()])]

           [(struct* :nl ())
            (cond
              [end-full? '()]
              [else
               (list (measure i
                              (cost-nl i)
                              (λ (tks) (list* "\n" (make-string i #\space) tks))))])]

           [(struct* :align ([d d])) (resolve d c c beg-full? end-full?)]

           [(struct* :nest ([n n] [d d])) (resolve d c (+ i n) beg-full? end-full?)]

           [(struct* :concat ([a a] [b b]))
            ;; analyze-left-ms :: bool? -> measure-set/c
            (define (analyze-left-ms mid-full?)
              (match (resolve a c i beg-full? mid-full?)
                [(? promise? a-mt)
                 (delay #:nl (doc-nl-cnt d)
                   (match (force a-mt)
                     ['() '()]
                     [(list a-m)
                      (match (extract-at-most-one
                              (resolve b (measure-last a-m) i mid-full? end-full?))
                        ['() '()]
                        [(list b-m) (list (concat-measure a-m b-m))])]))]
                [a-ms
                 ;; NOTE: Here, resolving `a` succeeds.
                 ;; We are now resolving `b` with many different `c` values,
                 ;; and concat the measures and merge them together:
                 ;;
                 ;; (merge
                 ;;   (analyze-right-ms (resolve b c1 ...))
                 ;;   (analyze-right-ms (resolve b c2 ...))
                 ;;   (analyze-right-ms (resolve b c3 ...))
                 ;;   ...)
                 ;;
                 ;; Due to Lemma 1, we can set #:prunable? to #t
                 ;; because (resolve b * ...)s will all fail or all succeed,
                 ;; so keeping only one tainted measure suffices.
                 (for/foldr ([ms-rest '()]) ([a-m (in-list a-ms)])
                   ;; analyze-right-ms :: measure-set/c -> measure-set/c
                   (define (analyze-right-ms b-ms)
                     (match b-ms
                       [(? promise?)
                        (delay #:nl (doc-nl-cnt d)
                          (match (force b-ms)
                            ['() '()]
                            [(list b-m) (list (concat-measure a-m b-m))]))]
                       ['() '()]
                       [(cons b-m b-ms)
                        (for/fold ([current-best (concat-measure a-m b-m)]
                                   [msr '()]
                                   #:result (reverse (cons current-best msr)))
                                  ([b-m (in-list b-ms)])
                          (define current (concat-measure a-m b-m))
                          (cond
                            [(cost<=? (measure-cost current)
                                      (measure-cost current-best))
                             (values current msr)]
                            [else (values current (cons current-best msr))]))]))

                   (merge (analyze-right-ms
                           (resolve b (measure-last a-m) i mid-full? end-full?))
                          ms-rest
                          #:prunable? #t))]))

            (merge (analyze-left-ms #f) (analyze-left-ms #t))]

           [(struct* :alternatives ([a a] [b b]))
            (merge (resolve a c i beg-full? end-full?)
                   (resolve b c i beg-full? end-full?))]

           [(struct* :cost ([n n] [d d]))
            (match (resolve d c i beg-full? end-full?)
              [(? promise? mt)
               (delay #:nl (doc-nl-cnt d)
                 (match (force mt)
                   ['() '()]
                   [(list m)
                    (list (struct-copy measure m [cost (cost+ (measure-cost m) n)]))]))]
              [ms
               (for/list ([m (in-list ms)])
                 (struct-copy measure m [cost (cost+ (measure-cost m) n)]))])]

           ;; This is essentially a dead code.
           ;; Partial evaluation should have removed all fails away already.
           [(struct* :fail ()) '()]))

       (define column-pos
         (match d
           [(struct* :text ([len len])) (+ c len)]
           [_ c]))

       (cond
         [(or (> column-pos limit) (> i limit))
          (delay #:nl (doc-nl-cnt d)
            (match (extract-at-most-one (core))
              ['()
               ((if beg-full?
                    (if end-full?
                        set-doc-failing/yes/yes!
                        set-doc-failing/yes/no!)
                    (if end-full?
                        set-doc-failing/no/yes!
                        set-doc-failing/no/no!)) d #t)
               '()]
              [result result]))]
         [else
          (define ret (core))
          ;; (printf "~a: ~a\n" d ret)
          ret]))))

  (define result
    (merge (resolve d offset offset #f #f) (resolve d offset offset #f #t)))

  (when (promise? result)
    (log-pretty-expressive-debug "tainted"))

  ;; NOTE: unlike OCaml, the doc d can be printed with other cost factories
  ;; so we need to reset the memoization table.
  (define cleanup-map (make-weak-hasheq))

  (define (cleanup d)
    (let loop ([d d])
      (when (doc-table/no/no d)
        (hash-clear! (doc-table/no/no d))
        (hash-clear! (doc-table/no/yes d))
        (hash-clear! (doc-table/yes/no d))
        (hash-clear! (doc-table/yes/yes d)))
      (hash-ref! cleanup-map d
                 (λ () (doc-process loop d)))))

  (match (begin0 (extract-at-most-one result)
           (cleanup d))
    ['() (raise (exn:fail:user "the document fails to print"
                               (current-continuation-marks)))]
    [(list m)
     (log-pretty-expressive-debug "cost: ~a" (measure-cost m))
     m]))

;; print-layout :: #:doc doc? -> #:factory cost-factory? -> string?
(define (print-layout #:doc d #:factory F #:offset offset)
  (string-append* ((measure-tok (print d F #:offset offset)) '())))

;; Lemma 1: the failure of resolving is independent of c and i.
;;          I.e., given d, c1, c2, i1, i2, beg-full?, and end-full?:
;;  (resolve d c1 i1 beg-full? end-full?) fails iff
;;  (resolve d c2 i2 beg-full? end-full?) fails
