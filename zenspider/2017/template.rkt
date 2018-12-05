#lang racket

(require "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (problem-NNa input)
  #f)

(module+ test
  (check-equal? (problem-NNa "123") 3))

(define (problem-NNb input)
  #f)

(module+ test
  (check-equal? (problem-NNb "123") 6))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file NN)))
  (problem-NNa input)
  (problem-NNb input))
