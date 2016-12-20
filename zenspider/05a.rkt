#lang racket/base

(require openssl/md5
         openssl/sha1
         racket/string)

(define (md5hash s n)
  (define x (md5-bytes (open-input-string (string-append s (number->string n)))))
  (and (zero? (bytes-ref x 0))
       (zero? (bytes-ref x 1))
       (zero? (bitwise-and #xF0 (bytes-ref x 2)))
       (let ([hex (bytes->hex-string x)])
         (substring hex 5 6))))

(define (unlock key len [start 0])
  (string-append*
   (reverse
    (for/fold ([l '()])
              ([i (in-naturals start)]
               #:break (>= (length l) len)
               #:when (md5hash key i))
      (cons (md5hash key i) l)))))

(module+ test
  (require rackunit)

  (check-equal? (md5hash "abc" 3231928) #f)
  (check-equal? (md5hash "abc" 3231929) "1")

  (check-equal? (unlock "abc" 0) "")
  (check-equal? (unlock "abc" 1 3231928) "1")
  ;; (check-equal? (unlock "abc" 1 3000000) "1")
  ;; (check-equal? (unlock "abc" 1) "1")
  ;; (check-equal? (unlock "abc" 2) "18")
  ;; (check-equal? (unlock "abc" 3) "18f")
  ;; (check-equal? (unlock "abc" 8) "18f47a30")

  ;; (check-equal? (unlock "wtnhxymk" 8) "2414bc77")
  )

(module+ main
  (time (displayln (unlock "wtnhxymk" 8))))
