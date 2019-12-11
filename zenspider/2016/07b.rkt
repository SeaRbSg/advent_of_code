#lang racket/base

(require racket/list
         racket/match
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

(define (ssl? ip)
  (define-values (hypers supers) (parse ip))
  (for*/or ([super supers]
            [sbytes (in-subs 3 (string->list super))]
            #:when (match-let* ([(list a b c) sbytes])
                     (and [eq? a c]
                          [not (eq? a b)]
                          (let ([target (list b a b)])
                            [for*/or ([hyper hypers]
                                     [hbytes (in-subs 3 (string->list hyper))])
                             (equal? hbytes target)]))))
    #t))

(module+ test
  (require rackunit)
  (check-equal? (tls? "abba[mnop]qrst") #t)
  (check-equal? (tls? "abcd[bddb]xyyx") #f)
  (check-equal? (tls? "aaaa[qwer]tyui") #f)
  (check-equal? (tls? "ioxxoj[asdfgh]zxcvbn") #t)

  (check-equal? (tls? "abba[mnop]qrst[bbbb]abcd") #t)
  (check-equal? (tls? "abba[mnop]qrst[abba]abcd") #f)

  ;; aba[bab]xyz supports SSL (aba outside square brackets with
  ;; corresponding bab within square brackets).
  (check-equal? (ssl? "aba[bab]xyz") #t)
  (check-equal? (ssl? "xyx[xyx]xyx") #f)
  (check-equal? (ssl? "aaa[kek]eke") #t)
  (check-equal? (ssl? "zazbz[bzb]cdb") #t)
  )

(module+ main
  (count-lines (data-file 7) tls?)
  (count-lines (data-file 7) ssl?))
