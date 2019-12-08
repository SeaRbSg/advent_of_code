#lang racket

(require syntax/parse/define)
(require "../2016/myutils.rkt")

;; TODO: (require "int-code.rkt")

(module+ test
  (require rackunit))

(define (mem-set mem pos v)
  (define v2 (vector-copy mem))         ; HACK: remove this
  (dbg "mem-set ~a = ~a~n" pos v)
  (vector-set! v2 pos v)
  v2)

(define (mem-get mem pos)
  (if (and (number? pos) (>= pos 0) (< pos (vector-length mem)))
      (begin
        (dbg "mem-get ~a = ~a~n" pos (vector-ref mem pos))
        (vector-ref mem pos))
      ;; (error 'mem-get "out of bounds ~a in ~a" pos mem)
      #f
      ))

(define mem-len vector-length)

(define (mem-peek mem pos)
  (define max (mem-len mem))

  (if (< pos max)
      (if (< pos (- max 5))
          (vector-take (vector-drop mem pos) 5)
          (vector-drop mem pos))
      (error 'mem-peek "Out of bounds entirely ~a < ~a" pos max)))

(define (b->i b) (if b 1 0))

;; (define-simple-macro (dbg fmt arg ...) (printf fmt arg ...))
(define-simple-macro (dbg fmt arg ...) (void))

(define (execute mem input)
  (define output null)
  (define max (mem-len mem))

  (for/fold ([mem mem]
             [pos 0]
             [done #f])
            ([steps (in-naturals)])
    #:break done
    (dbg "M ~a/~a ~a...~n" pos max (mem-peek mem pos))
    (let* ([inst (~0n (mem-get mem pos) 5)]
           [mc (string->number (substring inst 0 1))]
           [mb (string->number (substring inst 1 2))]
           [ma (string->number (substring inst 2 3))]
           [op (string->number (substring inst 3 5))]
           [raw1 (mem-get mem (+ 1 pos))]
           [raw2 (mem-get mem (+ 2 pos))]
           [raw3 (mem-get mem (+ 3 pos))]
           [r1 (if (zero? ma) (mem-get mem raw1) raw1)]
           [r2 (if (zero? mb) (mem-get mem raw2) raw2)]
           [r3 (if (zero? mc) (mem-get mem raw3) raw3)])
      (dbg "D inst:~a r1:~a r2:~a r3:~a~n" inst r1 r2 r3)
      (case op
        [(99)
         ;; =>
         (dbg "HALT ~a~n~n" (reverse output))
         (values                        ; HALT
          mem
          pos
          (not done))]

        [(01)                           ; ADD
         ;; =>
         (dbg "ADD ~a + ~a => ~a~n" r1 r2 raw3)
         (values (mem-set mem raw3 (+ r1 r2))
                 (+ 4 pos)
                 done)]

        [(02)                           ; MUL
         ;; =>
         (dbg "MUL ~a * ~a => ~a~n" r1 r2 raw3)
         (values (mem-set mem raw3 (* r1 r2))
                 (+ 4 pos)
                 done)]

        [(03)                           ; READ
         ;; =>
         (define v (car input))
         (set! input (cdr input))
         (dbg "READ: ~a => ~a~n" v raw1)
         (values (mem-set mem raw1 v)
                 (+ 2 pos)
                 done)]

        [(04)                           ; WRITE
         ;; =>
         (define v (mem-get mem raw1))
         (set! output (cons v output))
         (dbg "WRITE ~a => ~a~n" v output)
         (values mem
                 (+ 2 pos)
                 done)]

        [(05)                           ; JTRUE
         ;; =>
         (dbg "JTRUE ~a ~a~n" r1 r2)
         (values mem
                 (if (zero? r1) (+ 3 pos) r2)
                 done)]

        [(06)                           ; JFALSE
         ;; =>
         (dbg "JFALSE ~a ~a~n" r1 r2)
         (values mem
                 (if (zero? r1) r2 (+ 3 pos))
                 done)]

        [(07)                           ; LT
         ;; =>
         (dbg "LT ~a < ~a => ~a~n" r1 r2 r3)
         (values (mem-set mem raw3 (b->i (< r1 r2)))
                 (+ 4 pos)
                 done)]

        [(08)                           ; EQ
         ;; =>
         (dbg "EQ ~a = ~a => ~a~n" r1 r2 r3)
         (values (mem-set mem raw3 (b->i (eq? r1 r2)))
                 (+ 4 pos)
                 done)]

        [else (error 'execute "WTF?!? ~a ~a ~a ~a in ~a" mc mb ma op inst)])))

  (reverse output))

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
