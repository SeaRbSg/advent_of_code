#lang racket

(require "myutils.rkt")

(struct register (pc a b c d) #:transparent)
(define lookup (hasheq 'pc register-pc  ; UGH
                       'a  register-a
                       'b  register-b
                       'c  register-c
                       'd  register-d))
(define (get reg k)                     ; UGH
  ((hash-ref lookup k) reg))

(define (set reg k v)                   ; UGH! struct-copy doesn't work w/ vars
  (define pc (register-pc reg))
  (match k
    ['pc (struct-copy register reg       [pc (+ v pc)])]
    ['a  (struct-copy register reg [a v] [pc (+ 1 pc)])]
    ['b  (struct-copy register reg [b v] [pc (+ 1 pc)])]
    ['c  (struct-copy register reg [c v] [pc (+ 1 pc)])]
    ['d  (struct-copy register reg [d v] [pc (+ 1 pc)])]))

(define (run in [initial-values #f])
  (define (compile in)
    (define (n/s x) (or (string->number x) (string->symbol x)))
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "cpy" (app n/s x) (app n/s y)) `(cpy ,x ,y)]
        [(list "inc" (app n/s x))             `(inc ,x)]
        [(list "dec" (app n/s x))             `(dec ,x)]
        [(list "jnz" (app n/s x) (app n/s y)) `(jnz ,x ,y)])))

  (define ops (compile in))
  (define max (vector-length ops))
  (define pc 0)

  (define (execute reg)
    (define (n/v x)   (if (number? x) x (get reg x)))
    (define (cpy x y) (set reg y (n/v x)))
    (define (jnz x y) (set reg 'pc (if (zero? (n/v x)) 1 y)))
    (define (inc x)   (set reg x (add1 (n/v x))))
    (define (dec x)   (set reg x (sub1 (n/v x))))

    (define pc (get reg 'pc))

    (match (and (< pc max) (vector-ref ops pc))
      [(list 'cpy x y) (execute (cpy x y))]
      [(list 'inc x)   (execute (inc x))]
      [(list 'dec x)   (execute (dec x))]
      [(list 'jnz x y) (execute (jnz x y))]
      [_ reg]))

  (execute (apply register (or initial-values '(0 0 0 0 0)))))

(module+ test
  (require rackunit)

  (define in (open-input-string "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"))

  (check-equal? (run in)
                (register 6 42 0 0 0))

  (check-equal? (run (data-file 12))              ; 12a
                (register 23 318117 196418 0 0))

  (check-equal? (run (data-file 12) '(0 0 0 1 0)) ; 12b
                (register 23 9227771 5702887 0 0))
  )
