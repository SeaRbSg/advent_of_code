#lang racket/base

(require racket/list
         racket/match
         racket/string
         "myutils.rkt")

(define directions (parse-file (data-file 1)))

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

(module+ main
  (distance-to directions))

(module+ test
  (require rackunit)

  (check-equal? (distance-to "R8, R4, R4, R8") 4)

  (check-equal? (distance-to directions) 142)
  )
