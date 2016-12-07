#lang racket/base

(require racket/list
         racket/match
         racket/string
         "myutils.rkt")

(define directions "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3")

(define dirs  '([N . E] [E . S] [S . W] [W . N]))
(define rdirs '([E . N] [S . E] [W . S] [N . W]))

(define (split directions)
  (string-split directions ", "))

(define (convert directions)
  (for/list/flat ([inst (split directions)])
    (define-regexp (m (app string->number n)) #rx"(R|L)([0-9]+)" inst)
    (cons m (build-list n (lambda (_) "F")))))

(define (distance-to directions)
  (define-values (d x y c)
    (for/fold ([d 'N] [x 0] [y 0] [c '((0 . 0))])
              ([inst (convert directions)])
      #:break (check-duplicates c)
      (match-let* ([n 1]
                   [nd (match inst
                         ["F" d]
                         ["R" (cdr (assoc d dirs))]
                         ["L" (cdr (assoc d rdirs))])]
                   [nx (match nd ['N (+ x 1)] ['E x]       ['S (- x 1)] ['W x])]
                   [ny (match nd ['N y]       ['E (+ y 1)] ['S y]       ['W (- y 1)])]
                   [nc (cons (cons nx ny) c)])
        (if (equal? nd d)
            (values  d nx ny nc)
            (values nd  x  y  c)))))

  (+ (abs x) (abs y)))

(distance-to directions)

(module+ test
  (require rackunit)

  (check-equal? (distance-to "R8, R4, R4, R8") 4)

  (check-equal? (distance-to directions) 142))
