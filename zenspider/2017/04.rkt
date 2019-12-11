#lang racket/base

(require racket/list
         racket/string
         "../myutils.rkt")

(module+ test
  (require rackunit))

(define (run input valid?)
  (count-lines input valid?))

(module+ test
  (check-equal? 1 (run "aa bb cc dd ee" valid/a?))
  (check-equal? 1 (run "aa bb cc dd aaa" valid/a?))
  (check-equal? 0 (run "aa bb cc dd aa" valid/a?)))

(define (words input)
  (string-split input))

(module+ test
  (check-equal? '("a" "b" "c" "d") (words "a b c d")))

(define (uniq? words)
  (= (length words)
     (length (remove-duplicates words))))

(module+ test
  (check-equal? #t (uniq? '("aa" "bb" "cc" "dd" "ee")))
  (check-equal? #t (uniq? '("aa" "bb" "cc" "dd" "aaa")))
  (check-equal? #f (uniq? '("aa" "bb" "cc" "dd" "aa"))))

(define (valid/a? input)
  (uniq? (words input)))

(module+ test
  (check-equal? #t (valid/a? "aa bb cc dd ee"))
  (check-equal? #t (valid/a? "aa bb cc dd aaa"))
  (check-equal? #f (valid/a? "aa bb cc dd aa")))

(define (sort-chars str)
  (list->string (sort (string->list str) char<?)))

(define (anagram? words)
  (not (uniq? (map sort-chars words))))

(module+ test
  (check-equal? #f (anagram? (words "abcde fghij")))
  (check-equal? #t (anagram? (words "abcde xyz ecdab")))
  (check-equal? #f (anagram? (words "a ab abc abd abf abj")))
  (check-equal? #f (anagram? (words "iiii oiii ooii oooi oooo")))
  (check-equal? #t (anagram? (words "oiii ioii iioi iiio"))))

(define (valid/b? input)
  (not (anagram? (words input))))

(module+ test
  (check-equal? #t (valid/b? "abcde fghij"))
  (check-equal? #f (valid/b? "abcde xyz ecdab"))
  (check-equal? #t (valid/b? "a ab abc abd abf abj"))
  (check-equal? #t (valid/b? "iiii oiii ooii oooi oooo"))
  (check-equal? #f (valid/b? "oiii ioii iioi iiio")))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 4)))
  (run input valid/a?)
  (run input valid/b?))
