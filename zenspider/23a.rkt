#lang racket/base

(require racket/match
         "myutils.rkt")

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

(define (flip op)
  (match op
    [(list 'inc x)               `(dec ,x)]
    [(list 'dec x)               `(inc ,x)]
    [(list 'tgl x)               `(inc ,x)]
    [(list 'cpy x y)             `(jnz ,x ,y)]
    [(list 'jnz x (? symbol? y)) `(cpy ,x ,y)]
    [(list 'jnz x y)             `(nop ,x ,y)]
    [(list 'nop x y)             `(jnz ,x ,y)]))

(define (run in [initial-values #f])
  (define (compile in)
    (define num string->number)
    (define sym string->symbol)
    (define (n/s x) (or (string->number x) (string->symbol x)))
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "inc" (app sym x))             `(inc ,x)]
        [(list "dec" (app sym x))             `(dec ,x)]
        [(list "tgl" (app sym x))             `(tgl ,x)]
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
    (define (nop x y) (set reg 'pc 1))
    (define (tgl x)
      (define pc (+ (get reg x) (register-pc reg)))
      (define op (and (<= 0 pc (sub1 max)) (vector-ref ops pc)))
      (when op (vector-set! ops pc (flip op)))
      (set reg 'pc 1))

    (define pc (register-pc reg))
    (define op (and (< pc max) (vector-ref ops pc)))

    (match op
      [(list 'inc x)   (execute (inc x))]
      [(list 'dec x)   (execute (dec x))]
      [(list 'tgl x)   (execute (tgl x))]
      [(list 'nop x y) (execute (nop x y))]
      [(list 'cpy x y) (execute (cpy x y))]
      [(list 'jnz x y) (execute (jnz x y))]
      [#f reg]))

  (execute (apply register (or initial-values '(0 0 0 0 0)))))

(module+ test
  (require rackunit)

  (define in (open-input-string "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"))

  (check-equal? (run in)
                (register 6 42 0 0 0))

  (check-equal? (run (data-file 12))   ; 12a
                (register 23 318117 196418 0 0))

  (check-equal? (run (data-file 12) '(0 0 0 1 0)) ; 12b
                (register 23 9227771 5702887 0 0))

  ;; 23a
  (define in2 (open-input-string "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"))
  (check-equal? (run in2)
                (register 7 3 0 0 0))

  (check-equal? (run (data-file 23) '(0 7 0 0 0)) ; 23a
                (register 26 12330 1 0 0))

  ;; (check-equal? (run (data-file 23) '(0 12 0 0 0)) ; 23b -- 355301ms
  ;;               (register 26 479008890 1 0 0))
  )
