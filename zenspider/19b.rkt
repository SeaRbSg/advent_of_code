#lang racket

(define (logn n b)
  (/ (log n) (log b)))

(define (previous-power-of pow n)
  (if (zero? n) 0
      (expt pow (exact-floor (logn n pow)))))

(define % modulo)

(define (round! n) (floor (+ 1/2 n)))

(define (elf n)
  (define ppo3 (previous-power-of 3 n)) ; previous power of 3 (9 -> 3, 10 -> 9)
  (define diff (- n ppo3 1))            ; distance (m to n) in current segment
  (define frac (/ diff (* 2 ppo3)))     ; % length of current segment

  (+ (* [+ 1 (% (- n 1) ppo3)]          ; amount to move forwards
        [round (+ (- frac) 1)])         ; 1 if in 1st half of segment
     (* (+ n 1 (- ppo3) (% diff ppo3))  ; amount to move backwards
        [round! frac])))                ; 1 if in 2nd half of segment

(module+ test
  (require rackunit)

  (check-equal? (elf 2) 1)
  (check-equal? (elf 3) 3)
  (check-equal? (elf 4) 1)
  (check-equal? (elf 5) 2)
  (check-equal? (elf 6) 3)
  (check-equal? (elf 7) 5)
  (check-equal? (elf 8) 7)
  (check-equal? (elf 9) 9)
  (check-equal? (elf 10) 1)
  (check-equal? (elf 27) 27)
  (check-equal? (elf 28) 1)

  (check-equal? (elf 3017957) 1423634)
  )

(module+ main
  (string-join
   (for/list ([n (in-range 1 32)])
     (format "{~a, ~a}" n (elf n)))
   ", ")

  (elf 3017957)
  )
