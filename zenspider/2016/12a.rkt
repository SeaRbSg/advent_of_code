#lang racket/base

(require racket/match
         "../myutils.rkt")

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

(define (run in [initial-values #f])
  (define (compile in)
    (define num string->number)
    (define sym string->symbol)
    (define (n/s x) (or (num x) (sym x)))
    (for/vector ([inst (in-list (parse-lines-of-words in))])
      (match inst
        [(list "inc" (app sym x))             `(inc ,x)]
        [(list "dec" (app sym x))             `(dec ,x)]
        [(list "cpy" (app n/s x) (app sym y)) `(cpy ,x ,y)]
        [(list "jnz" (app n/s x) (app n/s y)) `(jnz ,x ,y)])))

  (define ops (compile in))
  (define max (vector-length ops))

  (define (execute reg)
    (define (n/v x)   (if (number? x) x (get reg x)))
    (define (cpy x y) (set reg y (n/v x)))
    (define (jnz x y) (set reg 'pc (if (zero? (n/v x)) 1 (n/v y))))
    (define (inc x)   (set reg x (add1 (n/v x))))
    (define (dec x)   (set reg x (sub1 (n/v x))))

    (define pc (register-pc reg))
    (define op (and (< pc max) (vector-ref ops pc)))

    (match op
      [(list 'inc x)   (execute (inc x))]
      [(list 'dec x)   (execute (dec x))]
      [(list 'cpy x y) (execute (cpy x y))]
      [(list 'jnz x y) (execute (jnz x y))]
      [#f reg]))

  (execute (apply register (or initial-values '(0 0 0 0 0)))))

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
