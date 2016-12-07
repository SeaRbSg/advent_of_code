#lang racket/base

(require racket/bool
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "myutils.rkt")

(define (cell n)                        ; builds '#(n u d l r), points to self
  (define v (make-vector 5))
  (vector-fill! v v)
  (vector-set! v 0 n)
  v)
(define (v c) (vector-ref c 0))
(define (u c) (vector-ref c 1))
(define (d c) (vector-ref c 2))
(define (l c) (vector-ref c 3))
(define (r c) (vector-ref c 4))
(define (wire-ud a b) (vector-set! a 2 b) (vector-set! b 1 a))
(define (wire-lr a b) (vector-set! a 4 b) (vector-set! b 3 a))

(define (ascii->pad ascii)
  (define pad
    (for/list ([row ascii])
      (for/list ([n (in-list row)])
        (if (equal? '_ n) #f (cell n)))))

  (for ([row pad])
    (for ([pair (in-subs 2 (filter-not false? row))])
      (apply wire-lr pair)))

  (for ([col (transpose pad)])
    (for ([pair (in-subs 2 (filter-not false? col))])
      (apply wire-ud pair)))

  (filter-not false? (flatten pad)))

(define ascii '((_ _ 1 _ _)
                (_ 2 3 4 _)
                (5 6 7 8 9)
                (_ A B C _)
                (_ _ D _ _)))

(define pad (ascii->pad ascii))
(define start (list-ref (flatten pad) (sub1 5)))

(define (decode start lines)
  (for/fold/1 ([acc ""]
               [pos start])
              ([line (string-split lines)])
    (define c (for/fold ([pos pos])
                        ([dir (in-string line)])
                (match dir
                  [#\U (u pos)]
                  [#\D (d pos)]
                  [#\L (l pos)]
                  [#\R (r pos)])))
    (values (string-append acc (~s (v c))) c)))

(define s (port->string (open-input-file "day02a.txt")))
(decode start s)

(module+ test
  (require rackunit)

  (check-equal? (decode start "ULL\nRRDDD\nLURDL\nUUUUD") "5DB3")
  (check-equal? (decode start s) "46C92")

  (printf "done~n"))
