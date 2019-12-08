#lang racket

(require "../2016/myutils.rkt")

;; (require syntax/parse/define)
;; (define-simple-macro (dbg fmt arg ...) (printf fmt arg ...))
;; (define-simple-macro (dbg fmt arg ...) (void))

;; TODO: (require "int-code.rkt")

(module+ test
  (require rackunit))

(define (mem-set mem pos v)
  (vector-set! mem pos v)
  mem)

(define (mem-get mem pos)
  (if (and (number? pos) (>= pos 0) (< pos (vector-length mem)))
      (begin
        (vector-ref mem pos))
      (error 'mem-get "out of bounds ~a > ~a in ~a" pos (vector-length mem) mem)))

(define mem-len vector-length)
(define mem-copy vector-copy)

(define (mem-peek mem pos)
  (define max (mem-len mem))

  (if (< pos max)
      (if (< pos (- max 5))
          (vector-take (vector-drop mem pos) 5)
          (vector-drop mem pos))
      (error 'mem-peek "Out of bounds entirely ~a < ~a" pos max)))

(define (b->i b) (if b 1 0))

(define (execute mem input)
  (define max (mem-len mem))

  (for/fold ([mem    (mem-copy mem)]
             [pos    0]
             [input  input]
             [output null]
             [done   #f]
             #:result (reverse output))
            ([steps (in-naturals)])
    #:break done
    (let* ([inst (rest (number->digits (+ 100000 (mem-get mem pos))))]
           [nth (λ (n) (list-ref inst n))]
           [mc (nth 0)]
           [mb (nth 1)]
           [ma (nth 2)]
           [op (+ (* 10 (nth 3)) (nth 4))] ; awkward?
           [raw1 (λ () (mem-get mem (+ 1 pos)))]
           [raw2 (λ () (mem-get mem (+ 2 pos)))]
           [raw3 (λ () (mem-get mem (+ 3 pos)))]
           [r1   (λ () (if (zero? ma) (mem-get mem (raw1)) (raw1)))]
           [r2   (λ () (if (zero? mb) (mem-get mem (raw2)) (raw2)))])
      (case op
        [(99)
         (values mem pos input output (not done))] ; HALT

        [(01)                                      ; ADD
         (values (mem-set mem (raw3) (+ (r1) (r2)))
                 (+ 4 pos)
                 input
                 output
                 done)]

        [(02)                                      ; MUL
         (values (mem-set mem (raw3) (* (r1) (r2)))
                 (+ 4 pos)
                 input
                 output
                 done)]

        [(03)                                      ; READ
         (define v (car input))
         (values (mem-set mem (raw1) v)
                 (+ 2 pos)
                 (cdr input)
                 output
                 done)]

        [(04)                                      ; WRITE
         (define v (mem-get mem (raw1)))
         (values mem
                 (+ 2 pos)
                 input
                 (cons v output)
                 done)]

        [(05)                                      ; JTRUE
         (values mem
                 (if (zero? (r1)) (+ 3 pos) (r2))
                 input
                 output
                 done)]

        [(06)                                      ; JFALSE
         (values mem
                 (if (zero? (r1)) (r2) (+ 3 pos))
                 input
                 output
                 done)]

        [(07)                                      ; LT
         (values (mem-set mem (raw3) (b->i (< (r1) (r2))))
                 (+ 4 pos)
                 input
                 output
                 done)]

        [(08)                                      ; EQ
         (values (mem-set mem (raw3) (b->i (eq? (r1) (r2))))
                 (+ 4 pos)
                 input
                 output
                 done)]

        [else (error 'execute "WTF?!? ~a ~a ~a ~a in ~a" mc mb ma op inst)]))))

(module+ test
  (define-simple-check (check-int-code input program output)
    (check-equal? (execute program input)
                  output))

  (check-int-code '(8) '#(3 9 8 9 10 9 4 9 99 -1 8) '(1))
  (check-int-code '(7) '#(3 9 8 9 10 9 4 9 99 -1 8) '(0))
  (check-int-code '(8) '#(3 9 7 9 10 9 4 9 99 -1 8) '(0))
  (check-int-code '(7) '#(3 9 7 9 10 9 4 9 99 -1 8) '(1))
  (check-int-code '(8) '#(3 3 1108 -1 8 3 4 3 99)   '(1))
  (check-int-code '(7) '#(3 3 1108 -1 8 3 4 3 99)   '(0))
  (check-int-code '(8) '#(3 3 1107 -1 8 3 4 3 99)   '(0))
  (check-int-code '(7) '#(3 3 1107 -1 8 3 4 3 99)   '(1)))

(define (problem-05a program)
  (execute program '(1)))

(define (problem-05b program)
  (execute program '(5)))

(module+ test
  (displayln 'done))

(module+ main
  (define input (list->vector (parse-numbers (data-file 05) ",")))
  (last (problem-05a input))
  (last (problem-05b input)))
