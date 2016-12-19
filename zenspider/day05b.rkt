#lang racket

(require openssl/md5)
(require openssl/sha1)
(require "myutils.rkt")

(define (md5hash s n [len 8])
  (define x (md5-bytes (open-input-string (string-append s (number->string n)))))
  (and (zero? (bytes-ref x 0))
       (zero? (bytes-ref x 1))
       (zero? (bitwise-and #xF0 (bytes-ref x 2)))
       (let* ([hex (bytes->hex-string x)]
              [idx (string->number (substring hex 5 6) 16)])
         (and (< idx len)
              (cons idx (substring hex 6 7))))))

(define (unlock key [len 8] [start 0])
  (define str (make-string len #\nul))
  (for ([i (in-naturals start)]
        #:break (for/none ([c (in-string str)]) (eq? #\nul c))
        #:when (md5hash key i len))
    (let* ([xy (md5hash key i len)]
           [pos (car xy)]
           [val (cdr xy)])
      (when (eq? #\nul (string-ref str pos))
        (string-set! str pos (string-ref val 0)))))
  str)

(module+ main
  (time (unlock "wtnhxymk" 1 27712455))
  (time (unlock "wtnhxymk" 8)))

(module+ test
  (require rackunit)

  (check-equal? (md5hash "abc" 3231928) #f)
  (check-equal? (md5hash "abc" 3231929) '(1 . "5"))
  (check-equal? (md5hash "abc" 5017308) #f)

  (check-equal? (unlock "abc" 0) "")
  ;; (check-equal? (unlock "abc" 1 8605827) "0")
  ;; (check-equal? (unlock "abc" 2 3231928) "05")  ; 28118ms
  ;; (check-equal? (unlock "abc" 3 3231928) "05a") ; 55896ms

  ;; (check-equal? (unlock "wtnhxymk" 8 2231253) "437e60fc") ; 144708ms
  )
