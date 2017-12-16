#lang racket/base

(require racket/list
         racket/string
         "myutils.rkt")

(define (decompress s)
  (define-regexp (b n m a) #px"([^(]*)(?:\\((\\d+)x(\\d+)\\))?(.*)" s)
  ;; (printf "~v: b=~v ~v ~v a=~v~n" s b n m a)
  (if (and n m)
      (let* ([n (string->number n)]
             [m (string->number m)]
             [a1 (substring a 0 n)]
             [a2 (substring a n)]
             [a1 (string-join (make-list m a1) "")])
        ;; (printf "  a1=~v a2=~v~n" a1 a2)
        ;; (string-append b a1 (decompress a2))
        (+ (string-length b)
           (string-length a1)
           (decompress a2)))
      (string-length b)))

(module+ test
  (require rackunit)

  (define-check (test in exp)
    ;; (newline)
    (check-equal? (decompress in) (if (string? exp) (string-length exp) exp)))

  (test "ADVENT"            "ADVENT")
  (test "A(1x5)BC"          "ABBBBBC")
  (test "(3x3)XYZ"          "XYZXYZXYZ")
  (test "A(2x2)BCD(2x2)EFG" "ABCBCDEFEFG")
  (test "(6x1)(1x3)A"       "(1x3)A")
  (test "X(8x2)(3x3)ABCY"   "X(3x3)ABC(3x3)ABCY")

  (test (string-trim (parse-file (data-file 9))) 152851)
  )

(module+ main
  (decompress (string-trim (parse-file (data-file 9))))
  )
