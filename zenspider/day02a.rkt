#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/port
         racket/sequence
         racket/string)

(define (transpose m) (apply map list (sequence->list m)))
(define (in-subs n l)                   ; like ruby's each_cons(n)
  (make-do-sequence
   (lambda ()
     (values (lambda (xs) (take xs n))
             cdr l
             (lambda (xs) (pair? (drop xs (sub1 n))))
             #f #f))))

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
(define (wire-ud l a b) (vector-set! a 2 b) (vector-set! b 1 a))
(define (wire-lr l a b) (vector-set! a 4 b) (vector-set! b 3 a))

(define pad (build-list 9 (lambda (n) (cell (add1 n)))))
(define start (list-ref pad (sub1 5)))

(for ([row (in-slice 3 pad)])
  (for/list ([pair (in-subs 2 row)])
    (apply wire-lr pad pair)))

(for ([col (transpose (in-slice 3 pad))])
  (for/list ([pair (in-subs 2 col)])
    (apply wire-ud pad pair)))

(define-syntax (values/first stx)       ; returns first value given to it
  (syntax-case stx ()
    [(_ values-expr) #'(first (call-with-values (λ () values-expr) list))]))

(define-syntax (for/fold/1 stx)         ; only returns first value of for/fold
  (syntax-case stx ()
    [(_ body ...) #'(values/first (for/fold body ...))]))

(define (decode start lines)
  (for/fold/1 ([acc 0]
               [pos start])
              ([line (string-split lines)])
    (define c (for/fold ([pos pos])
                        ([dir (in-string line)])
                (match dir
                  [#\U (u pos)]
                  [#\D (d pos)]
                  [#\L (l pos)]
                  [#\R (r pos)])))
    (values (+ (* 10 acc) (v c)) c)))

(decode start (call-with-input-file "day02a.txt" port->string))

(module+ test
  (require rackunit)

  (check-equal? (decode start "ULL\nRRDDD\nLURDL\nUUUUD") 1985)

  (printf "done~n"))
