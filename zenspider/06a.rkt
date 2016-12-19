#lang racket/base

(require racket/file
         racket/list
         "myutils.rkt")

(define alphabet (string->list "abcdefghijklmnopqrstuvwxyz"))

(define (noise lines)
  (list->string
   (for/list ([col (transpose (map string->list lines))])
     (argmax (lambda (c) (count (lambda (y) (equal? c y)) col)) alphabet))))

(module+ main
  (noise (parse-lines (data-file 6))))

(module+ test
  (require rackunit
           racket/string)

  (define s (string-split "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"))

  (check-equal? (noise s) "easter")
  )
