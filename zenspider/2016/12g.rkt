#lang racket/base

(require racket/match
         "myutils.rkt")

(struct register (pc a b c d) #:transparent)

(define (get reg k)
  (match k
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

  (define (optimize ops)
    (define n? number?)
    (define s? symbol?)
    (define z? zero?)
    (for/vector ([op (in-vector ops)])
      (match op
        [(list 'inc x)                 (λ (reg) (set reg x (add1 (get reg x))))]
        [(list 'dec x)                 (λ (reg) (set reg x (sub1 (get reg x))))]
        [(list 'cpy (? n? x) y)        (λ (reg) (set reg y x))]
        [(list 'cpy x y)               (λ (reg) (set reg y (get reg x)))]
        [(list 'jnz (? n? x) (? n? y)) (λ (reg) (set reg 'pc (if (z? x) 1 y)))]
        [(list 'jnz (? s? x) (? n? y)) (λ (reg) (set reg 'pc (if (z? (get reg x)) 1 y)))])))

  (define ops (compile in))
  (define cache (optimize ops))
  (define max (vector-length ops))

  (define (execute reg)
    (define pc (register-pc reg))
    (define op (and (< pc max) (vector-ref cache pc)))

    (if op
        (execute (op reg))
        reg))

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
