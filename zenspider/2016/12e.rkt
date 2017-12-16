#lang racket

(require "myutils.rkt")

(struct register (pc a b c d) #:mutable #:transparent)

(define (set reg k v)                   ; UGH! struct-copy doesn't work w/ vars
  (define pc (register-pc reg))
  (match k
    ['pc (struct-copy register reg       [pc (+ v pc)])]
    ['a  (struct-copy register reg [a v] [pc (+ 1 pc)])]
    ['b  (struct-copy register reg [b v] [pc (+ 1 pc)])]
    ['c  (struct-copy register reg [c v] [pc (+ 1 pc)])]
    ['d  (struct-copy register reg [d v] [pc (+ 1 pc)])]))

(define (n/v reg x) (if (number? x) x (match x
                                        ['a (register-a reg)]
                                        ['b (register-b reg)]
                                        ['c (register-c reg)]
                                        ['d (register-d reg)])))
(define (cpy reg x y) (set reg y (n/v reg x)))
(define (inc reg x)   (set reg x (add1 (n/v reg x))))
(define (dec reg x)   (set reg x (sub1 (n/v reg x))))
(define (jnz reg x y)
  (set-register-pc! reg (+ (register-pc reg) (if (zero? (n/v reg x)) 1 y)))
  reg)

(define (run in [initial-values #f])
  (define (next reg)
    (define pc (register-pc reg))
    (match (and (< pc max) (vector-ref ops pc))
      [(? procedure? f) (f reg)]
      [#f reg]))

  (define (compile in)
    (define (n/s x) (or (string->number x) (string->symbol x)))
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "cpy" (app n/s x) (app n/s y)) (lambda (reg) (next (cpy reg x y)))]
        [(list "inc" (app n/s x))             (lambda (reg) (next (inc reg x)))]
        [(list "dec" (app n/s x))             (lambda (reg) (next (dec reg x)))]
        [(list "jnz" (app n/s x) (app n/s y)) (lambda (reg) (next (jnz reg x y)))])))

  (define ops (compile in))
  (define max (vector-length ops))

  (next (apply register (or initial-values '(0 0 0 0 0))))
  )

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
