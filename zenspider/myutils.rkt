#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/port
         racket/sequence
         racket/string)

(provide (all-defined-out))

(define-syntax-rule (values/first values-expr)
  (first (call-with-values (lambda () values-expr) list)))

(define-syntax-rule (for/fold/1 body ...) ; only returns first value of for/fold
  (values/first (for/fold body ...)))

(define-syntax-rule (for/list/flat body ...)
  (flatten (for/list body ...)))

(define-syntax-rule (define-regexp (pat ...) re str)
  (match-define (regexp re (list _ pat ...)) str))

(define (in-subs n l)                   ; like ruby's each_cons(n)
  (make-do-sequence
   (lambda ()
     (values (lambda (xs) (take xs n))
             cdr l
             (lambda (xs) (pair? (drop xs (sub1 n))))
             #f #f))))

(define (flatmap f lst)
  (apply append (map f lst)))

(define (groups-of n l)
  (sequence->list (in-slice 3 (flatten l))))

(define (parse-lines-of-numbers in)
  (for/list ([line (port->lines in)])
    (map string->number (string-split line))))

(define (rotate lls)
  (groups-of 3 (transpose lls)))

(define (transpose m) (apply map list (sequence->list m)))
