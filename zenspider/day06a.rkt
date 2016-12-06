#lang racket/base

(require racket/file
         racket/list)

(define (my-transpose m) (apply map list m))
(define alphabet (string->list "abcdefghijklmnopqrstuvwxyz"))

(define (noise lines)
  (list->string
   (for/list ([col (my-transpose (map string->list lines))])
     (argmax (lambda (c) (count (lambda (y) (equal? c y)) col)) alphabet))))

(noise (file->lines "day06a.txt"))

(module+ test
  (require rackunit
           racket/string)

  (define s (string-split "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"))

  (check-equal? (noise s) "easter")
  (printf "done~n")
  )