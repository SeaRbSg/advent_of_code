#lang racket/base

(require racket/list
         racket/port
         racket/sequence
         racket/string)

(provide (all-defined-out))

(define (groups-of n l)
  (sequence->list (in-slice 3 (flatten l))))

(define (parse-lines-of-numbers in)
  (for/list ([line (port->lines in)])
    (map string->number (string-split line))))

(define (rotate lls)
  (groups-of 3 (transpose lls)))

(define (transpose m) (apply map list (sequence->list m)))
