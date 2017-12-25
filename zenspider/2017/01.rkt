#lang racket/base

(require racket/sequence
         racket/string
         "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (problem-01 input offset-proc)
  (define numbers (string->digits (string-trim input)))
  (for/sum ([a (in-list numbers)]
            [b (sequence-tail (in-cycle numbers)
                              (offset-proc numbers))])
    (if (eq? a b) a 0)))

(define (problem-01a input)
  (problem-01 input (λ _arg 1)))

(module+ test
  (check-equal? (problem-01a "1122")     3)
  (check-equal? (problem-01a "1111")     4)
  (check-equal? (problem-01a "1234")     0)
  (check-equal? (problem-01a "91212129") 9))

(define (problem-01b input)
  (problem-01 input (λ (ns) (floor (/ (length ns) 2)))))

(module+ test
  (check-equal? (problem-01b "1212")      6)
  (check-equal? (problem-01b "1221")      0)
  (check-equal? (problem-01b "123425")    4)
  (check-equal? (problem-01b "123123")   12)
  (check-equal? (problem-01b "12131415")  4))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 1)))
  (problem-01a input)
  (problem-01b input))
