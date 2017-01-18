#lang racket

(require rackunit)
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define-syntax-rule (while condition body ...)
                    (let loop ()
                      (when condition
                        body ...
                        (loop))))

(define cpu (vector 0 0 0 0))
(define pc 0)

(define (register-index register)
  (case register
    ['a 0]
    ['b 1]
    ['c 2]
    ['d 3]))

(define (set-register register value)
  (vector-set! cpu (register-index register) value))

(define (get-register register)
  (vector-ref cpu (register-index register)))

(define (inc x)
  (set-register x (add1 (get-register x))))

(define (dec x)
  (set-register x (sub1 (get-register x))))

(define (cpy x y)
  (set-register y x))

(define (jnz x y)
  (when (not (zero? (get-register x)))
    (set! pc (+ pc y))))

(define (run p)
  (while (< pc (vector-length p))
         (eval (vector-ref p pc) ns)
         (set! pc (add1 pc))))

(define program
  (vector
  '(cpy 41 'a)
  '(inc 'a)
  '(inc 'a)
  '(dec 'a)
  '(jnz 'a 2)
  '(dec 'a)))

(println program)
(run program)
(println cpu)
