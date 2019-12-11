#lang racket/base

(require racket/list
         "../myutils.rkt")

(define (parse ip)                      ; -> hypers supers
  (partition (lambda (s) (eq? (string-ref s 0) #\[))
             (regexp-match* #px"\\[\\w+\\]|\\w+" ip)))


(define (abba? s)
  (for/or ([bytes (in-subs 4 (string->list s))])
    (define-list (a b c d) bytes)
    (and (eq? a d)
         (eq? b c)
         (not (eq? a b)))))

(define (tls? ip)
  (define-values (hypers supers) (parse ip))
  (and (ormap abba? supers)
       (nonemap abba? hypers)))

(module+ test
  (require rackunit)
  (check-equal? (tls? "abba[mnop]qrst") #t)
  (check-equal? (tls? "abcd[bddb]xyyx") #f)
  (check-equal? (tls? "aaaa[qwer]tyui") #f)
  (check-equal? (tls? "ioxxoj[asdfgh]zxcvbn") #t)

  (check-equal? (tls? "abba[mnop]qrst[bbbb]abcd") #t)
  (check-equal? (tls? "abba[mnop]qrst[abba]abcd") #f)
  )

(module+ main
  (count-lines (data-file 7) tls?))
