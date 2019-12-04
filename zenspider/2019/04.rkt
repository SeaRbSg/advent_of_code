#lang racket

;; ## --- Day 4: Secure Container ---
;;
;; You arrive at the Venus fuel depot only to discover it's protected by a
;; password.  The Elves had written the password on a sticky note, but someone
;; _threw it out_ ((Look on the bright side - isn't it more secure if nobody
;; knows the password?)).
;;
;; However, they do remember a few key facts about the password:
;;
;; * It is a six-digit number.
;; * The value is within the range given in your puzzle input.
;; * Two adjacent digits are the same (like `22` in `1_22_345`).
;; * Going from left to right, the digits _never decrease_; they only ever
;;   increase or stay the same (like `111123` or `135679`).
;;
;; Other than the range rule, the following are true:
;;
;; * `111111` meets these criteria (double `11`, never decreases).
;; * `2234_50_` does not meet these criteria (decreasing pair of digits `50`).
;; * `123789` does not meet these criteria (no double).
;;
;; _How many different passwords_ within the range given in your puzzle input
;; meet these criteria?

(require "../2016/myutils.rkt")

(module+ test
  (require rackunit))

(define (problem-04a input)
  #f)

(module+ test
  (check-equal? (problem-04a "123") 3))

(define (problem-04b input)
  #f)

(module+ test
  (check-equal? (problem-04b "123") 6))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 04)))
  (problem-04a input)
  (problem-04b input))
