#lang racket

(require "../myutils.rkt")

(module+ test
  (require rackunit))

(define (parse input)
  (for/hash ([pair (parse-lines-of-atoms input ")")])
    (values (second pair) (first pair))))

(define (count orbits name)
  (define sub (hash-ref orbits name #f))
  (if sub (add1 (count orbits sub)) 0))

(define (problem-06a input)
  (define orbits (parse input))
  (for/sum ([name (in-hash-keys orbits)])
    (count orbits name)))

(define (path orbits name)
  (define sub (hash-ref orbits name #f))
  (if sub
      (cons sub (path orbits sub))
      empty))

(define (count-path orbits from to)
  (define a (path orbits from))
  (define b (path orbits to))
  (define c (last (set-intersect a b)))  ; last might be wrong

  (define d (count orbits from))
  (define e (count orbits to))
  (define f (- (count orbits c)))

  (+ d e f f))

(module+ test
  (define input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")

  (define parsed (parse input))

  (check-equal? parsed
                '#hash((B . COM)
                       (C . B)
                       (D . C)
                       (E . D)
                       (F . E)
                       (G . B)
                       (H . G)
                       (I . D)
                       (J . E)
                       (K . J)
                       (L . K)))

  (check-equal? (count parsed 'L)
                7)

  (check-equal? (problem-06a input)
                42))

(define (problem-06b input)
  (- (count-path (parse input) 'YOU 'SAN) 2))

(module+ test
  (check-equal? (path parsed 'L)
                '(K J E D C B COM))

  (check-equal? (count-path parsed 'K 'I)
                4))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 06)))
  (problem-06a input)
  (problem-06b input))
