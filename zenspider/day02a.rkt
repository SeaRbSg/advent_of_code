#lang racket

(define (decode lines)
  (define-values (acc _)
    (for/fold ([acc 0]
               [pos 5])
              ([line (string-split lines)])
      (define n (for/fold ([pos pos])
                          ([dir (in-string line)])
                  (match dir
                    [#\U (if (< (- pos 3) 1)      pos (- pos 3))]
                    [#\D (if (> (+ pos 3) 9)      pos (+ pos 3))]
                    [#\L (if (= 1 (modulo pos 3)) pos (- pos 1))]
                    [#\R (if (= 0 (modulo pos 3)) pos (+ pos 1))])))
      (values (+ (* 10 acc) n) n)))
  acc)

(decode (call-with-input-file "day02a.txt" port->string))

(module+ test
  (require rackunit)

  (check-equal? (decode "ULL\nRRDDD\nLURDL\nUUUUD") 1985)

  (printf "done~n"))
