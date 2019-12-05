#lang racket/base

(require racket/function
         racket/list
         "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (crack/1a n)
  (define ds (number->digits n))        ; extract digits (O(n))

  (and
   (check-duplicates ds)                ; walk for dups (O(n))
   (equal? ds (sort ds <))))            ; sort and compare (O(log n) + O(n))

(define (crack/1b n)
  (for/fold ([dup #f]                   ; use fold to do all at once
             [asc #t]                   ; ascending and duplicate checking
             [last -1]
             #:result (and dup asc))
            ([n (in-list (number->digits n))]) ; still extracting digits
    (values (or  dup (= n last))
            (and asc (<= last n))
            n)))

(define (crack/1c n)
  (let loop ([num   n]                  ; use let loop to extract digits
             [dup  #f]                  ; and do descending and duplicate checks
             [dec  #t]                  ; descending because left to right
             [last 99])
    (if (zero? num)
        (and dup dec)
        (let ([m (quotient num 10)]
              [n (remainder num 10)])
          (loop m
                (or  dup (= n last))
                (and dec (<= n last))
                n)))))

(define (crack/1d n)
  (let loop ([num   n]
             [last 99]
             [dup  #f]
             [dec  #t])
    (if (or (zero? num) (not dec))      ; short circuit if ever ascending
        (and dup dec)
        (let ([m (quotient num 10)]
              [n (remainder num 10)])
          (loop m
                n
                (or  dup (=  n last))
                (and dec (<= n last)))))))

(define crack crack/1d)

(define (run s pred)
  (define-list (a b) (car (parse-lines-of-numbers s "-")))

  (for/count ([n (in-range a b)])
    (pred n)))

(define (problem-04a input)
  (run input crack))

(module+ test
  (define-simple-check (check-truthy x) (check-equal? (not (not x)) #t))
  (define-simple-check (check-falsey x) (check-equal? (not x) #t))

  (define (check-crack/1 fn)
    (check-truthy (fn 111111))
    (check-falsey (fn 223450))
    (check-falsey (fn 123789))
    (check-equal? (run "158126-624574" fn) 1665))

  (check-crack/1 crack/1a)
  (check-crack/1 crack/1b)
  (check-crack/1 crack/1c)
  (check-crack/1 crack/1d)

  (check-equal? (problem-04a "158126-624574") 1665))

;; crack/1d:   65.000  i/s
;; crack/1c:   35.000  i/s -     1.86  x slower
;; crack/1b:   25.000  i/s -     2.60  x slower
;; crack/1a:    5.000  i/s -    13.00  x slower

(module+ profile/1
  (require benchmark-ips)
  (benchmark/ips
   "crack/1a" (run "158126-624574" crack/1a)
   "crack/1b" (run "158126-624574" crack/1b)
   "crack/1c" (run "158126-624574" crack/1c)
   "crack/1d" (run "158126-624574" crack/1d)))

(define (crack/2a n)
  (define ds (number->digits n))

  (and
   (check-duplicates ds)
   (equal? ds (sort ds <))
   (ormap (lambda (ns) (= 2 (length ns)))
          (group-by identity ds))))

(define (crack/2d n)
  (let loop ([num   n]
             [dup  #f]
             [dec  #t]
             [last 99])
    (if (or (zero? num) (not dec))
        (and dup dec
             (ormap (lambda (ns) (= 2 (length ns)))
                    (group-by identity (number->digits n))))
        (let ([m (quotient num 10)]
              [n (remainder num 10)])
          (loop m
                (or  dup (= n last))
                (and dec (<= n last))
                n)))))

(define crack/2 crack/2d)

(define (problem-04b input)
  (run input crack/2))

(module+ test
  (define (check-crack/2 fn)
    (check-truthy (fn 112233))
    (check-falsey (fn 123444))
    (check-truthy (fn 111122))

    (check-equal? (run "158126-624574" fn) 1131))

  (check-crack/2 crack/2a)
  (check-crack/2 crack/2d)

  (check-equal? (problem-04b "158126-624574\n") 1131))

;; crack/2d:   59.000  i/s
;; crack/2a:    6.000  i/s -     9.83  x slower

(module+ profile/2
  (require benchmark-ips)
  (benchmark/ips
   "crack/2a" (run "158126-624574" crack/2a)
   "crack/2d" (run "158126-624574" crack/2d)))

;; 04a:   70.000  i/s
;; 04b:   60.000  i/s -     1.17  x slower

(module+ profile/top
  (require benchmark-ips)
  (benchmark/ips
   "04a" (problem-04a "158126-624574")
   "04b" (problem-04b "158126-624574")))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 04)))
  (problem-04a input)
  (problem-04b input))
