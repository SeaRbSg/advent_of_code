#lang racket

(require "myutils.rkt")

(struct register (pc a b c d) #:transparent)

(define (get reg k)                     ; UGH
  (match k
    ['pc (register-pc reg)]
    ['a (register-a reg)]
    ['b (register-b reg)]
    ['c (register-c reg)]
    ['d (register-d reg)]))

(define (set reg k v)                   ; UGH! struct-copy doesn't work w/ vars
  (define pc (register-pc reg))
  (match k
    ['pc (struct-copy register reg       [pc (+ v pc)])]
    ['a  (struct-copy register reg [a v] [pc (+ 1 pc)])]
    ['b  (struct-copy register reg [b v] [pc (+ 1 pc)])]
    ['c  (struct-copy register reg [c v] [pc (+ 1 pc)])]
    ['d  (struct-copy register reg [d v] [pc (+ 1 pc)])]))

(define (n/v reg x)   (if (number? x) x (get reg x)))
(define (cpy reg x y) (set reg y (n/v reg x)))
(define (jnz reg x y) (set reg 'pc (if (zero? (n/v reg x)) 1 y)))
(define (inc reg x)   (set reg x (add1 (n/v reg x))))
(define (dec reg x)   (set reg x (sub1 (n/v reg x))))

(define (run in [initial-values #f])
  (define (compile in)
    (define (n/s x) (or (string->number x) (string->symbol x)))
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "cpy" (app n/s x) (app n/s y)) (lambda (reg) (cpy reg x y))]
        [(list "inc" (app n/s x))             (lambda (reg) (inc reg x))]
        [(list "dec" (app n/s x))             (lambda (reg) (dec reg x))]
        [(list "jnz" (app n/s x) (app n/s y)) (lambda (reg) (jnz reg x y))])))

  (define ops (compile in))
  (define max (vector-length ops))

  (let loop ([reg (apply register (or initial-values '(0 0 0 0 0)))])
    (define pc (register-pc reg))
    (if (< pc max)
        (loop ((vector-ref ops pc) reg))
        reg)))

(module+ test
  (require rackunit)

  (define in (open-input-string "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"))

  (time (check-equal? (run in)
                      (register 6 42 0 0 0)))

  (time (check-equal? (run (data-file 12))   ; 12a
                      (register 23 318117 196418 0 0)))

  (time (check-equal? (run (data-file 12) '(0 0 0 1 0)) ; 12b
                      (register 23 9227771 5702887 0 0)))
  )
