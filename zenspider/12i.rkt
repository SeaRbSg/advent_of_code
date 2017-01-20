#lang br/quicklang

(provide cpy jnz inc dec
         ops pc a b c d
         ops! a! b! c! d!
         next
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
                       (ops! (list ,@ops))
                       ;; (c! 1)           ; 12b init
                       (time (void (next)))
                       `((pc ,pc) (a ,a) (b ,b) (c ,c) (d ,d)))))

(define (compile port)
  (for/list ([line (in-lines port)]
             #:when (non-empty-string? line))
    (format-datum '(~a) line)))

;;; registers & memory

(define ops #f)
(define ops-max #f)
(define pc 0)
(define a 0)
(define b 0)
(define c 0)
(define d 0)

(define (ops! v)
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

(module+ test
  (require rackunit)
  (require check-sexp-equal)

  (check-equal? (compile (open-input-string "inc a"))
                '((inc a)))

  (check-sexp-equal? (test-reader read-syntax "inc a\ndec z")
                '(module cpu-mod "12i.rkt"
                   (ops! (list (inc a) (dec z)))
                   (time (void (next)))
                   `((pc ,pc) (a ,a) (b ,b) (c ,c) (d ,d)))))
