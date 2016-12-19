#lang racket

(require racket/match
         "myutils.rkt")

(define (triangle? l)
  (match-define (list a b c) (sort l <))

  (> (+ a b) c))

(module+ main
  (for/sum ([l (parse-lines-of-numbers (data-file 3))])
    (if (triangle? l) 1 0)))

(module+ test
  (require rackunit)

  (check-equal? (triangle? '(3 4 5)) #t)
  (check-equal? (triangle? '(5 10 25)) #f)
  (check-equal? (triangle? '(10 5 25)) #f)
  )
