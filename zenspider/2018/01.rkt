#lang racket/base

(require "../myutils.rkt")

(define nums (time (parse-numbers (data-file 1))))

(define (problem1)
  (apply + nums))

(define (problem2)
  (for/fold ([freq 0]
             [seen (hash)]
             #:result freq)
            ([n (in-cycle nums)])
    (define m (+ freq n))
    #:final (hash-ref seen m #f)
    (values m
            (hash-set seen m #t))))

;; problem 1
(time (problem1))

;; problem 2
(time (problem2))
