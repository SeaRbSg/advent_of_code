#lang racket

(require "../myutils.rkt")

(struct register (pc a b c d) #:transparent)

(define (fwd reg [n 1])
  (struct-copy register reg [pc (+ n (register-pc reg))]))

(define (f reg [n 1])
  (+ n (register-pc reg)))

(define (register-cpyna reg x) (struct-copy register reg [a x] [pc (f reg)]))
(define (register-cpynb reg x) (struct-copy register reg [b x] [pc (f reg)]))
(define (register-cpync reg x) (struct-copy register reg [c x] [pc (f reg)]))
(define (register-cpynd reg x) (struct-copy register reg [d x] [pc (f reg)]))

;; HACK: only what is needed for my input
(define (register-cpyac reg)   (struct-copy register reg [a (register-c reg)] [pc (f reg)]))
(define (register-cpyba reg)   (struct-copy register reg [b (register-a reg)] [pc (f reg)]))
(define (register-cpybc reg)   (struct-copy register reg [b (register-c reg)] [pc (f reg)]))
(define (register-cpyca reg)   (struct-copy register reg [c (register-a reg)] [pc (f reg)]))

(define (register-inca reg)    (struct-copy register reg [a (add1 (register-a reg))] [pc (f reg)]))
(define (register-incb reg)    (struct-copy register reg [b (add1 (register-b reg))] [pc (f reg)]))
(define (register-incc reg)    (struct-copy register reg [c (add1 (register-c reg))] [pc (f reg)]))
(define (register-incd reg)    (struct-copy register reg [d (add1 (register-d reg))] [pc (f reg)]))

(define (register-deca reg)    (struct-copy register reg [a (sub1 (register-a reg))] [pc (f reg)]))
(define (register-decb reg)    (struct-copy register reg [b (sub1 (register-b reg))] [pc (f reg)]))
(define (register-decc reg)    (struct-copy register reg [c (sub1 (register-c reg))] [pc (f reg)]))
(define (register-decd reg)    (struct-copy register reg [d (sub1 (register-d reg))] [pc (f reg)]))

(define (register-jnza reg y)  (struct-copy register  reg [pc (f reg (if (zero? (register-a reg)) 1 y))]))
(define (register-jnzb reg y)  (struct-copy register  reg [pc (f reg (if (zero? (register-b reg)) 1 y))]))
(define (register-jnzc reg y)  (struct-copy register  reg [pc (f reg (if (zero? (register-c reg)) 1 y))]))
(define (register-jnzd reg y)  (struct-copy register  reg [pc (f reg (if (zero? (register-d reg)) 1 y))]))

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
    (for/vector ([inst (parse-lines-of-words in)])
      (match inst
        [(list "cpy" (?app num x) y)           (fn/2 (fn/1 "register-cpyn~a" y) x)]
        [(list "cpy" x y)                      (fn/1 "register-cpy~a~a" y x)]
        [(list "inc" x)                        (fn/1 "register-inc~a" x)]
        [(list "dec" x)                        (fn/1 "register-dec~a" x)]
        [(list "jnz" (?app num x) (app num y)) (fn/2 register-jmp (if (= 0 x) 1 y))]
        [(list "jnz" x (app num y))            (fn/2 (fn/1 "register-jnz~a" x) y)]
        )))

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
