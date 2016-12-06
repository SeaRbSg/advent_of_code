#lang racket/base

(require racket/match
         racket/string)

(define directions "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3")

(define dirs  '([N . E] [E . S] [S . W] [W . N]))
(define rdirs '([E . N] [S . E] [W . S] [N . W]))

(define (split directions)
  (string-split directions ", "))

(define (convert directions)
  (split directions))

(define (distance-to directions)
  (define-values (d x y)
   (for/fold ([d 'N] [x 0] [y 0])
             ([inst (convert directions)])
     (match-let* ([(regexp #rx"(R|L)([0-9]+)"
                           (list _ m (app string->number n))) inst]
                  [nd (match m
                        ["R" (cdr (assoc d dirs))]
                        ["L" (cdr (assoc d rdirs))])]
                  [nx (match nd ['N (+ x n)] ['E x]       ['S (- x n)] ['W x])]
                  [ny (match nd ['N y]       ['E (+ y n)] ['S y]       ['W (- y n)])])
       (values nd nx ny))))

  (+ (abs x) (abs y)))

(distance-to directions)

(module+ test
  (require rackunit)

  (check-equal? (distance-to "R2, L3") 5)
  (check-equal? (distance-to "R2, R2, R2") 2)
  (check-equal? (distance-to "R5, L5, R5, R3") 12)

  (check-equal? (distance-to directions) 243))
