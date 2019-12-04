#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/runtime-path
         racket/sequence
         racket/set
         racket/string)

(require (only-in racket/unsafe/undefined [unsafe-undefined none]))

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (flatten/1 lists)
  (apply append lists))

(define-syntax-rule (define-list (pat ...) expr)
  (match-define (list pat ...) expr))

(define-syntax-rule (values/first values-expr)
  (first (call-with-values (lambda () values-expr) list)))

(define-syntax-rule (for/fold/1 body ...) ; only returns first value of for/fold
  (values/first (for/fold body ...)))

(define-syntax-rule (for/list/flat body ...)
  (flatten (for/list body ...)))

(define-syntax-rule (for/list/min body ...)
  (apply min (for/list body ...)))

(define-syntax-rule (for/all  body ...) (for/and body ...))
(define-syntax-rule (for/none body ...) (not (for/or body ...)))

(define-syntax for/count
  (syntax-rules ()
    [(_ (vars ...) body ...+) (for/count (vars ...) (begin body ...+))] ; =>
    [(_ (vars ...) body)      (for/sum (vars ... #:when body) 1)]))

(define-syntax-rule (define-regexp (pat ...) re str)
  (match-define (regexp re (list _ pat ...)) str))

(define (in-subs n l)                   ; like ruby's each_cons(n)
  (make-do-sequence
   (lambda ()
     (values (lambda (xs) (take xs n))
             cdr l
             (lambda (xs) (pair? (drop xs (sub1 n))))
             #f #f))))

(define char->number
  (let ([zero (char->integer #\0)])
    (λ (c) (- (char->integer c) zero))))

(module+ test
  (check-equal? (char->number #\3) 3))

(define (number->digits num [base 10])
  (let loop ([num    num]
             [result null])
    (if (zero? num) result
        (loop (quotient num base)
              (cons (remainder num base) result)))))

(module+ test
  (check-equal? (number->digits 1234567890) '(1 2 3 4 5 6 7 8 9 0)))

(define (string->digits s)
  (map char->number (string->list s)))

(module+ test
  (check-equal? (string->digits "1234") '(1 2 3 4)))

(define (count-lines in f)
  (for/sum ([line (parse-lines in)])
    (if (f line) 1 0)))

(define-runtime-path DIR (find-system-path 'orig-dir))
;; TODO: Can't figure out how to use DIR at compile-time expansion
(define-runtime-path INPUT (build-path (find-system-path 'orig-dir) "input"))

(define (data-file n)
  (define filename (string-append (~0n n 2) ".txt"))
  (define f1 (build-path DIR   filename))
  (define f2 (build-path INPUT filename))

  (if (file-exists? f1) f1 f2))

(define (flatmap f lst)
  (apply append (map f lst)))

(define (group-by-map f l)              ; more like ruby's group_by
  (map (lambda (xs) (cons (f (first xs)) xs)) (group-by f l)))

(define (groups-of n l)
  (sequence->list (in-slice n (flatten l))))

(define (nonemap f l)
  (not (ormap f l)))

(define (occur l)
  (sort (group-by-map length (group-by identity l))
        (lambda (a b) (< (first b) (first a)))))

(define (list-intersect . xss)
  (set->list (apply set-intersect (map list->set xss))))

(define (->port in)
  (cond [(input-port? in) in]
        [(file-exists? in) (open-input-file in)]
        [else (open-input-string in)]))

(define (parse-file in)
  (port->string (->port in)))

(define (parse-lines in)
  (port->lines (->port in)))

(define (parse-lines-of-numbers in [sep none])
  (for/list ([line (parse-lines in)])
    (map string->number (string-split line sep))))

(define (parse-numbers in [sep none])
  (flatten (parse-lines-of-numbers in sep)))

(define (parse-lines-of-words in [sep none])
  (for/list ([line (parse-lines in)])
    (string-split line sep)))

(define (parse-words in [sep none])
  (flatten (parse-lines-of-words in sep)))

(define (parse-lines-of-atoms in [sep none])
  (define (string-or-symbol s) (or (string->number s) (string->symbol s)))
  (for/list ([line (parse-lines in)])
    (map string-or-symbol (string-split line sep))))

(define (rotate lls)
  (groups-of 3 (transpose lls)))

(define (transpose m) (apply map list (sequence->list m)))

(define (~0n n [w 1])
  (~n n w "0"))

(define (~n n [w 1] [p " "])
  (~a n #:align 'right #:min-width w #:pad-string p))

(define (~f flt [w 1] [d 2])
  (~r flt #:min-width w #:precision d))

(define (~f! flt [p 3])
  (string->number (real->decimal-string flt p)))

(module+ test
  (displayln 'done))
