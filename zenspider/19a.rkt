#lang racket

(define (logn n b)
  (/ (log n) (log b)))

(define (previous-power-of pow n)
  (if (zero? n) 0
      (expt pow (exact-floor (logn n pow)))))

(define (elf n)
  (+ 1 (* 2 (- n (previous-power-of 2 n)))))

(module+ test
  (require rackunit)

  (check-equal? (elf 2) 1)
  (check-equal? (elf 3) 3)
  (check-equal? (elf 4) 1)
  (check-equal? (elf 5) 3)
  (check-equal? (elf 6) 5)
  (check-equal? (elf 7) 7)
  (check-equal? (elf 8) 1)
  (check-equal? (elf 9) 3)

  (check-equal? (elf 16) 1)
  (check-equal? (elf 17) 3)

  (check-equal? (elf 3017957) 1841611)
  )

(module+ main
  (string-join
   (for/list ([n (in-range 1 32)])
     (format "{~a, ~a}" n (elf n)))
   ", ")

  (elf 3017957)
  )
