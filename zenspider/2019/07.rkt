#lang racket

(require "../myutils.rkt")
(require "int_code.rkt")

(module+ test
  (require rackunit))

(define (run-amp mem phases [input '(0)])
  (for/fold ([input input]
             #:result (first input))
            ([phase (in-list phases)])
    (execute mem (cons phase input))))

(define (problem-07a input)
  (define mem (mem-parse input))

  (for/fold ([max (void)])
            ([phases (in-permutations (range 0 5))])
    (let ([result (cons (run-amp mem phases) (list phases))])
      (cond [(void? max) result]
            [(< (car max) (car result)) result]
            [else max]))))

(module+ test
  (define-simple-check (check-int-code/5 input program output)
    (check-equal? (execute program input)
                  output))

  (define input/1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
  (define input/2 "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
  (define input/3 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")

  (check-equal? (problem-07a input/1)
                '(43210 (4 3 2 1 0)))

  (check-equal? (problem-07a input/2)
                '(54321 (0 1 2 3 4)))

  (check-equal? (problem-07a input/3)
                '(65210 (1 0 4 3 2))))

(define (problem-07b input)
  #;
  (argmax car
         (for/list ([phases (sequence->list (in-permutations '(range 5 10)))])
           (cons (run-amp mem phases) (list phases))))
  #f)

(module+ test
  (check-equal? (problem-07b "123") 6))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 07)))
  (problem-07a input)
  (problem-07b input))
