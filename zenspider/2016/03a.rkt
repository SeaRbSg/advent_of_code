#lang racket/base

(require racket/match
         "../myutils.rkt")

(define (triangle? l)
  (match-define (list a b c) (sort l <))
  (> (+ a b) c))

(define (go)
  (for/count ([l (in-list (parse-lines-of-numbers (data-file 3)))])
    (triangle? l)))

(module+ main
  (go))

(module+ test
  (require rackunit)

  (check-equal? (triangle? '(3 4 5)) #t)
  (check-equal? (triangle? '(5 10 25)) #f)
  (check-equal? (triangle? '(10 5 25)) #f)
  (check-equal? (go) 993)
  )
