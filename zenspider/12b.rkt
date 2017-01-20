#lang racket/base

(require (for-syntax racket/base)
         racket/match
         "myutils.rkt")

(struct register (pc a b c d) #:mutable #:transparent)

(define (fwd reg [n 1])
  (set-register-pc! reg (+ n (register-pc reg))))

(define (register-cpyna reg x) (set-register-a! reg x) (fwd reg))
(define (register-cpynb reg x) (set-register-b! reg x) (fwd reg))
(define (register-cpync reg x) (set-register-c! reg x) (fwd reg))
(define (register-cpynd reg x) (set-register-d! reg x) (fwd reg))

;; HACK: only what is needed for my input
(define (register-cpyac reg)   (set-register-a! reg (register-c reg)) (fwd reg))
(define (register-cpyba reg)   (set-register-b! reg (register-a reg)) (fwd reg))
(define (register-cpybc reg)   (set-register-b! reg (register-c reg)) (fwd reg))
(define (register-cpyca reg)   (set-register-c! reg (register-a reg)) (fwd reg))

(define (register-inca reg)    (set-register-a! reg (add1 (register-a reg))) (fwd reg))
(define (register-incb reg)    (set-register-b! reg (add1 (register-b reg))) (fwd reg))
(define (register-incc reg)    (set-register-c! reg (add1 (register-c reg))) (fwd reg))
(define (register-incd reg)    (set-register-d! reg (add1 (register-d reg))) (fwd reg))

(define (register-deca reg)    (set-register-a! reg (sub1 (register-a reg))) (fwd reg))
(define (register-decb reg)    (set-register-b! reg (sub1 (register-b reg))) (fwd reg))
(define (register-decc reg)    (set-register-c! reg (sub1 (register-c reg))) (fwd reg))
(define (register-decd reg)    (set-register-d! reg (sub1 (register-d reg))) (fwd reg))

(define (register-jnza reg y)  (fwd reg (if (zero? (register-a reg)) 1 y)))
(define (register-jnzb reg y)  (fwd reg (if (zero? (register-b reg)) 1 y)))
(define (register-jnzc reg y)  (fwd reg (if (zero? (register-c reg)) 1 y)))
(define (register-jnzd reg y)  (fwd reg (if (zero? (register-d reg)) 1 y)))

(define (register-jmp  reg y) (fwd reg y))

(define-match-expander ?app
  (lambda (stx)
    (syntax-case stx ()
      [(_ fn vars ...)
       #'(? fn (app fn vars ...))])))

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

(define (run in [initial-values #f])
  (define (compile in)
    (define (fn/1 fmt . args)
      (namespace-variable-value (string->symbol (apply format fmt args)) #t #f ns))
    (define (fn/2 f x)
      (lambda (reg) (f reg x)))
    (define num string->number)
    (define sym string->symbol)
    (for/vector ([inst (in-list (parse-lines-of-words in))])
      (match inst
        [(list "inc" x)                        (fn/1 "register-inc~a" x)]
        [(list "dec" x)                        (fn/1 "register-dec~a" x)]
        [(list "cpy" (?app num x) y)           (fn/2 (fn/1 "register-cpyn~a" y) x)]
        [(list "cpy" x y)                      (fn/1 "register-cpy~a~a" y x)]
        [(list "jnz" (?app num x) (app num y)) (fn/2 register-jmp (if (= 0 x) 1 y))]
        [(list "jnz" x (app num y))            (fn/2 (fn/1 "register-jnz~a" x) y)]
        )))

  (define ops (compile in))
  (define max (vector-length ops))

  (define reg (apply register (or initial-values '(0 0 0 0 0))))

  (let loop ()
    (define pc (register-pc reg))
    (if (< pc max)
        (begin ((vector-ref ops pc) reg)
               (loop))
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
