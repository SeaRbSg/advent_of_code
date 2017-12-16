#lang br/quicklang

;; 2945 % racket -e '(require (submod "12i.rkt" test))'
;; Segmentation fault: 11

(provide cpy jnz inc dec
         ops pc a b c d result
         ops! a! b! c! d!
         next
         compile run
         read-syntax
         #%module-begin
         (all-from-out br/quicklang)    ; basically racket/base
         )

;;; support for: #lang reader "12i.rkt"

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define ops (compile port))
  (datum->syntax #f `(module cpu-mod "12i.rkt"
                       (provide pc a b c d result)
                       (run (list ,@ops)))))

(define (compile port)
  (for/list ([line (in-lines port)]
             #:when (and (non-empty-string? line)
                         (not (string-prefix? line ";"))))
    (format-datum '(~a) line)))

(define (run opcodes [initial-values #f])
  (ops! (maybe-expand opcodes) initial-values)

  (time (void (next)))
  (set! result `((pc ,pc) (a ,a) (b ,b) (c ,c) (d ,d)))

  result)

(define-namespace-anchor nsa)           ; for maybe-expand

(define ns (and (getenv "BUG") (namespace-anchor->namespace nsa)))

(define (maybe-expand v)
  (parameterize ([current-namespace (namespace-anchor->namespace nsa)])
    (for/list ([op (in-list v)])
      (if (procedure? op) op    ; lets us use directly or as #lang
          (eval (expand op))))))

;;; registers & memory

(define result #f)
(define ops #f)
(define ops-max #f)
(define-values (pc a b c d) (values 0 0 0 0 0))

(define (ops! v [initial-values #f])
  (if initial-values
      (set!-values (pc a b c d) (apply values initial-values))
      (set!-values (pc a b c d) (values 0 0 0 0 0)))

  (set! ops (list->vector v))
  (set! ops-max (vector-length ops)))
(define (pc+ n) (set! pc (+ pc n)))
(define (pc++) (pc+ 1))
(define (a! v) (set! a v))
(define (b! v) (set! b v))
(define (c! v) (set! c v))
(define (d! v) (set! d v))

;;; macros for ops

(begin-for-syntax
  (define (setter id) (format-id id "~a!" id)))

(define-syntax (cpy stx)
  (syntax-case stx ()
    [(cpy A B) #`(thunk (#,(setter #'B) A)        (pc++) (next))]))

(define-syntax-rule (jnz A B)
  (thunk                (pc+ (if (zero? A) 1 B))         (next)))

(define-syntax (inc stx)
  (syntax-case stx ()
    [(inc A)   #`(thunk (#,(setter #'A) (add1 A)) (pc++) (next))]))

(define-syntax (dec stx)
  (syntax-case stx ()
    [(_ A)     #`(thunk (#,(setter #'A) (sub1 A)) (pc++) (next))]))

(define (next)
  (and (< pc ops-max) ((vector-ref ops pc))))

(module* test racket        ; avoid module+, causes circularity issues
  (require rackunit)
  (require (prefix-in my/ (submod ".."))) ; explicity require w/ prefix
  (require (only-in "myutils.rkt" data-file))

  (define (run port [initial #f])
    (when (path? port)
      (set! port (open-input-file port)))
    (my/run (my/compile port) initial))
  (define (register pc a b c d)
    `((pc ,pc) (a ,a) (b ,b) (c ,c) (d ,d)))

  (define in (open-input-string "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"))

  (time (check-equal? (run in)
                      (register 6 42 0 0 0)))

  (time (check-equal? (run (data-file 12))   ; 12a
                      (register 23 318117 196418 0 0)))

  (time (check-equal? (run (data-file 12) '(0 0 0 1 0)) ; 12b
                      (register 23 9227771 5702887 0 0)))

  (time (for ([_ (in-range 10)]) (run (data-file 12) '(0 0 0 1 0))))

  (displayln 'done))
