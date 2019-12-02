#lang racket

(require syntax/parse/define)
(require "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (execute ops)
  (for ([quad (in-slice 4 ops)])
    #:break (= 99 (first quad))
    (match quad
      ([list  1 a b dst]
       ;; =>
       (vector-set! ops dst (+ (vector-ref ops a)
                               (vector-ref ops b))))
      ([list  2 a b dst]
       ;; =>
       (vector-set! ops dst (* (vector-ref ops a)
                               (vector-ref ops b))))))
  ops)

(define (tweak ops noun verb)
  (define new (vector-copy ops))
  (vector-set! new 1 noun)
  (vector-set! new 2 verb)
  new)

(define (problem-02a input)
  (vector-ref (execute (tweak input 12 2)) 0))

(module+ test
  (define-simple-check (check-execute input expect)
    (check-equal? (execute (vector-copy input))
                  expect))

  (check-execute '#(1 0 0 0 99)
                 '#(2 0 0 0 99))

  (check-execute '#(2 3 0 3 99)
                 '#(2 3 0 6 99))

  (check-execute '#(2 4 4 5 99 0)
                 '#(2 4 4 5 99 9801))

  (check-execute '#( 1 1 1 4 99 5 6 0 99)
                 '#(30 1 1 4 2 5 6 0 99))

  (check-execute '#(   1 9 10  3 2 3 11 0 99 30 40 50)
                 '#(3500 9 10 70 2 3 11 0 99 30 40 50)))

(define (problem-02b ops)
  (define (run a b) (vector-ref (execute (tweak ops a b)) 0))

  (for*/first ([a (in-range 1 100)]
               [b (in-range 1 100)]
               #:when (= (run a b) 19690720))
    (+ (* 100 a) b)))

(module+ test
  (displayln 'done))

(module+ main
  (define input (list->vector (parse-numbers (data-file 02) ",")))

  (problem-02a input)
  (problem-02b input))
