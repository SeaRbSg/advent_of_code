#lang racket

(define-syntax (values/first stx)       ; returns first value given to it
  (syntax-case stx ()
    [(_ values-expr) #'(first (call-with-values (λ () values-expr) list))]))

(define-syntax (for/fold/1 stx)         ; only returns first value of for/fold
  (syntax-case stx ()
    [(_ body ...) #'(values/first (for/fold body ...))]))

(define (decode lines)
  (for/fold/1 ([acc 0]
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

(decode (call-with-input-file "day02a.txt" port->string))

(module+ test
  (require rackunit)

  (check-equal? (decode "ULL\nRRDDD\nLURDL\nUUUUD") 1985)

  (printf "done~n"))
