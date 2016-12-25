#lang racket

(require "myutils.rkt")

(struct register (pc a b c d) #:transparent)

(define computer (register 0 0 0 0 0))

(define lookup (hasheq 'pc register-pc
                       'a register-a
                       'b register-b
                       'c register-c
                       'd register-d))

(define (up reg k v)                    ; UGH! struct-copy doesn't work w/ vars
  (define pc (register-pc reg))

  (match k
    ['pc (struct-copy register reg [pc (+ v pc)])]
    ['a  (struct-copy register reg [a v] [pc (add1 pc)])]
    ['b  (struct-copy register reg [b v] [pc (add1 pc)])]
    ['c  (struct-copy register reg [c v] [pc (add1 pc)])]
    ['d  (struct-copy register reg [d v] [pc (add1 pc)])]))

(define (run in)
  (define (compile in)
    (define (n/s x) (or (string->number x) (string->symbol x)))
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "cpy" (app n/s x) (app n/s y)) `(cpy ,x ,y)]
        [(list "inc" (app n/s x))             `(inc ,x)]
        [(list "dec" (app n/s x))             `(dec ,x)]
        [(list "jnz" (app n/s x) (app n/s y)) `(jnz ,x ,y)])))

  (define ops (compile in))
  (define ops-max (vector-length ops))
  (define pc 0)

  (define (execute reg)
    (define (n/v x) (if (number? x) x ((hash-ref lookup x) reg)))
    (define (cpy x y) (up reg y (n/v x)))
    (define (jnz x y) (up reg 'pc (if (zero? (n/v x)) 1 y)))
    (define (inc x)   (up reg x (add1 (n/v x))))
    (define (dec x)   (up reg x (sub1 (n/v x))))

    (define pc (register-pc reg))
    (define op (and (< pc ops-max) (vector-ref ops pc)))

    (match op
      [(list 'cpy x y) (execute (cpy x y))]
      [(list 'inc x)   (execute (inc x))]
      [(list 'dec x)   (execute (dec x))]
      [(list 'jnz x y) (execute (jnz x y))]
      [_ reg]))

  (execute (register 0 0 0 0 0)))

(module+ test
  (require rackunit)

  (define in (open-input-string "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"))

  (check-equal? (run in)
                (register 6 42 0 0 0))

  (check-equal? (run (data-file 12))
                (register 23 318117 196418 0 0))

  )
