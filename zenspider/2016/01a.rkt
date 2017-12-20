#lang racket/base

(require racket/match
         racket/string
         "myutils.rkt")

(define directions (parse-file (data-file 1)))

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
     (define-regexp (m (app string->number n)) #rx"(R|L)([0-9]+)" inst)
     (match-let* ([nd (match m
                        ["R" (cdr (assoc d dirs))]
                        ["L" (cdr (assoc d rdirs))])]
                  [nx (match nd ['N (+ x n)] ['E x]       ['S (- x n)] ['W x])]
                  [ny (match nd ['N y]       ['E (+ y n)] ['S y]       ['W (- y n)])])
       (values nd nx ny))))

  (+ (abs x) (abs y)))

(module+ main
  (distance-to directions))

(module+ test
  (require rackunit)

  (check-equal? (distance-to "R2, L3") 5)
  (check-equal? (distance-to "R2, R2, R2") 2)
  (check-equal? (distance-to "R5, L5, R5, R3") 12)

  (check-equal? (distance-to directions) 243)
  )