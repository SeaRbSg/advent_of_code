#lang racket

(define (triangle? l)
  (match-define (list a b c) (sort l <))

  (> (+ a b) c))

(for/sum ([line (file->lines "day03a.txt")])
  (if (triangle? (map string->number (string-split line)))
      1
      0))

(module+ test
  (require rackunit)

  (check-equal? (triangle? '(3 4 5)) #t)
  (check-equal? (triangle? '(5 10 25)) #f)
  (check-equal? (triangle? '(10 5 25)) #f)

  (displayln "done"))
