#lang racket

(require "../myutils.rkt")
(require "int_code.rkt")

(module+ test
  (require rackunit))

(module+ test
  (define-simple-check (check-int-code input program output)
    (check-equal? (execute program input)
                  output))

  (check-int-code '(8) '#(3 9 8 9 10 9 4 9 99 -1 8) '(1))
  (check-int-code '(7) '#(3 9 8 9 10 9 4 9 99 -1 8) '(0))
  (check-int-code '(8) '#(3 9 7 9 10 9 4 9 99 -1 8) '(0))
  (check-int-code '(7) '#(3 9 7 9 10 9 4 9 99 -1 8) '(1))
  (check-int-code '(8) '#(3 3 1108 -1 8 3 4 3 99)   '(1))
  (check-int-code '(7) '#(3 3 1108 -1 8 3 4 3 99)   '(0))
  (check-int-code '(8) '#(3 3 1107 -1 8 3 4 3 99)   '(0))
  (check-int-code '(7) '#(3 3 1107 -1 8 3 4 3 99)   '(1)))

(define (problem-05a program)
  (execute program '(1)))

(define (problem-05b program)
  (execute program '(5)))

(module+ test
  (displayln 'done))

(module+ main
  (define input (list->vector (parse-numbers (data-file 05) ",")))
  (last (problem-05a input))
  (last (problem-05b input)))
