#lang racket

(require "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (parse input)
  (for/list ([line (parse-lines-of-words input ",")])
    (for/list ([ins (in-list line)])
      (define-regexp (dir (app string->number n)) #px"([UDLR])([0-9]+)" ins)
      (match dir
        ["R" (list (+ n)    0)]
        ["L" (list (- n)    0)]
        ["U" (list    0  (+ n))]
        ["D" (list    0  (- n))]))))

(define (expand coords)
  (for/list ([line (in-list coords)])
    (flatten/1
     (for/list ([coord (in-list line)])
       (match coord
         [(list x 0) (make-list (abs x) (list (* (sgn x) 1) 0))]
         [(list 0 y) (make-list (abs y) (list 0 (* (sgn y) 1)))])))))

(define (walk coords)
  (for/list ([line (in-list coords)])
    (for/fold ([acc '((0 0))]
               #:result (rest (reverse acc)))
              ([coord (in-list line)])
      (let* ([curr (first acc)]
             [x  (first  curr)]
             [y  (second curr)]
             [dx (first  coord)]
             [dy (second coord)])
        (cons (list (+ x dx) (+ y dy)) acc)))))

(define (problem-03a input)
  (define-list (walk-a walk-b) (walk (expand (parse input))))
  (for/list/min ([xy (in-list (list-intersect walk-a walk-b))])
    (+ (abs (first xy))
       (abs (second xy)))))

(module+ test
  (define input1 "R8,U5,L5,D3\nU7,R6,D4,L4\n")
  (define input2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
  (define input3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  (check-equal? (parse input1)
                '(((8 0) (0 5) (-5 0) (0 -3))
                  ((0 7) (6 0) (0 -4) (-4 0))))

  (check-equal? (expand (parse input1))
                '([(1 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0)
                   (0 1) (0 1) (0 1) (0 1) (0 1)
                   (-1 0) (-1 0) (-1 0) (-1 0) (-1 0)
                   (0 -1) (0 -1) (0 -1)]
                  [(0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1)
                   (1 0) (1 0) (1 0) (1 0) (1 0) (1 0)
                   (0 -1) (0 -1) (0 -1) (0 -1)
                   (-1 0) (-1 0) (-1 0) (-1 0)]))

  (check-equal? (first (walk (expand (parse input1))))
                '(
                  (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0)
                  (8 1) (8 2) (8 3) (8 4) (8 5)
                  (7 5) (6 5) (5 5) (4 5) (3 5)
                  (3 4) (3 3) (3 2)))

  (check-equal? (problem-03a input1)
                6)

  (check-equal? (problem-03a input2)
                159)

  (check-equal? (problem-03a input3)
                135))

(define (sum-steps line-a line-b)
  (for/list/min ([coord (in-list (list-intersect line-a line-b))])
    (+ (add1 (index-of line-a coord))
       (add1 (index-of line-b coord)))))

(define (problem-03b input)
  (define-list (walk-a walk-b) (walk (expand (parse input))))
  (sum-steps walk-a walk-b))

(module+ test
  (check-equal? (problem-03b input1)
                30)

  (check-equal? (problem-03b input2)
                610)

  (check-equal? (problem-03b input3)
                410))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 03)))
  (problem-03a input)
  (problem-03b input))
