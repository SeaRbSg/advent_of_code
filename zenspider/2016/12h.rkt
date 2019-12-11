#lang racket/base

(require racket/match
         "../myutils.rkt")

(struct register (pc a b c d) #:mutable #:transparent)

(define (run in [initial-values #f])
  (define reg (apply register (or initial-values '(0 0 0 0 0))))

  (define (getter k)
    (match k
      ['pc (λ () (register-pc reg))]
      ['a  (λ () (register-a  reg))]
      ['b  (λ () (register-b  reg))]
      ['c  (λ () (register-c  reg))]
      ['d  (λ () (register-d  reg))]))

  (define (setter k)
    (match k
      ['pc (λ (v) (set-register-pc! reg (+ v (register-pc reg))))]
      ['a  (λ (v) (set-register-a! reg v) (set-register-pc! reg (+ 1 (register-pc reg))))]
      ['b  (λ (v) (set-register-b! reg v) (set-register-pc! reg (+ 1 (register-pc reg))))]
      ['c  (λ (v) (set-register-c! reg v) (set-register-pc! reg (+ 1 (register-pc reg))))]
      ['d  (λ (v) (set-register-d! reg v) (set-register-pc! reg (+ 1 (register-pc reg))))]
      ))

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
        [(list 'inc x)                 (define g (getter x)) (define s (setter x))   (λ () (s (add1 (g))))]
        [(list 'dec x)                 (define g (getter x)) (define s (setter x))   (λ () (s (sub1 (g))))]
        [(list 'cpy (? n? x) y)        (define s (setter y))                         (λ () (s x))]
        [(list 'cpy x y)               (define g (getter x)) (define s (setter y))   (λ () (s (g)))]
        [(list 'jnz (? n? x) (? n? y)) (define s (setter 'pc))                       (λ () (s (if (z? x) 1 y)))]
        [(list 'jnz (? s? x) (? n? y)) (define g (getter x)) (define s (setter 'pc)) (λ () (s (if (z? (g)) 1 y)))])))

  (define ops (optimize (compile in)))
  (define max (vector-length ops))

  (let loop ()
    (define pc (register-pc reg))
    (when (< pc max)
      ((vector-ref ops pc))
      (loop)))
  reg)

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
