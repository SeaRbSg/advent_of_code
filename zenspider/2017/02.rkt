#lang racket

(require math/number-theory
         math/base
         "../myutils.rkt")

(module+ test
  (require rackunit))

(define (minmax numbers)           ; TODO: make more efficient w/ fold
  (list (apply min numbers) (apply max numbers)))

(module+ test
  (check-equal? (minmax '(42))
                '(42 42))
  (check-equal? (minmax '(0 1 2 8 9 10))
                '(0 10)))

(define (minmax-diff numbers)
  (abs (apply - (minmax numbers))))

(module+ test
  (check-equal? (minmax-diff '(42))
                0)
  (check-equal? (minmax-diff '(0 1 2 8 9 10))
                10))

(define (problem-02a input)
  (sum (map minmax-diff (parse-lines-of-numbers input))))

(module+ test
  (check-equal? (problem-02a "5 1 9 5\n7 5 3\n2 4 6 8\n")
                18))

(define (problem-02b input)
  (sum (map minmax-diff (parse-lines-of-numbers input)))
)

;;   def run input
;;     lines_of_numbers(input).map { |a| # should use triangle enumeration
;;       min, max = a.select { |n|
;;         a.any? { |m|
;;           n != m && ((((m / n) * n) == m) || (((n / m) * m) == n))
;;         }
;;       }.minmax
;;       max / min
;;     }.sum
;;   end

(module+ test
  (check-equal? (problem-02b "0 10\n1 3\n5 7")
                6)
  (check-equal? (problem-02b "5 9 2 8\n9 4 7 3\n3 8 6 5")
                9))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 2)))
  (problem-02a input)
  (problem-02b input))
