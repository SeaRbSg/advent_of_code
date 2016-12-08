#lang racket/base

(require racket/bool
         racket/list
         racket/string
         "myutils.rkt")

(define (parse-room-name s)
  (define-regexp (enc (app string->number sec) chk) #rx"([a-z-]+)([0-9]+)\\[([a-z]+)\\]" s)
  (list enc sec chk))

(define (valid? encoded checksum)
  (define sorted (occur (string->list (string-append* (string-split encoded "-")))))
  (define subsorted (map (lambda (l) (sort (map first (cdr l)) char<?)) sorted))
  (define signature (list->string (take (flatten subsorted) 5)))
  (equal? signature checksum))

(define (sum-of-rooms names)
  (for/sum ([name names])
    (define-list (a b c) (parse-room-name name))
    (if (valid? a c)
        b
        0)))

(define (decode-name s n)
  (define char-base (char->integer #\a))
  (define (char+n c n)
    (integer->char
     (+ char-base (modulo (+ n (- (char->integer c) char-base)) 26))))

  (string-trim (list->string (for/list ([c (string->list s)])
                               (if (char=? c #\-)
                                   #\space
                                   (char+n c n))))))

(define (valid-rooms names)
  (filter-not false? (for/list ([name names])
                       (define-list (a b c) (parse-room-name name))
                       (if (valid? a c)
                           (list a b)
                           #f))))

(define (decode-valid-rooms names)
  (map (lambda (ab) (cons (apply decode-name ab) (cdr ab)))
       (valid-rooms names)))

(module+ test
  (require racket/file
           rackunit)

  (define (is-valid? name)
    (define-list (a _ c) (parse-room-name name))
    (valid? a c))

  (define-simple-check (check-valid? name exp)
    (equal? (is-valid? name) exp))

  (define name "aaaaa-bbb-z-y-x-123[abxyz]")

  (define-list (a b c) (parse-room-name name))
  (check-equal? a "aaaaa-bbb-z-y-x-")
  (check-equal? b 123)
  (check-equal? c "abxyz")

  (check-equal? (valid? a c) #t)

  (check-equal? (sum-of-rooms '()) 0)

  (define rooms '("aaaaa-bbb-z-y-x-123[abxyz]"
                  "a-b-c-d-e-f-g-h-987[abcde]"
                  "not-a-real-room-404[oarel]"
                  "totally-real-room-200[decoy]"))

  (define-list (a2 b2 c2) (parse-room-name (second rooms)))
  (check-equal? a2 "a-b-c-d-e-f-g-h-")
  (check-equal? b2 987)
  (check-equal? c2 "abcde")

  (check-equal? (map is-valid? rooms) '(#t #t #t #f))

  (check-equal? (sum-of-rooms '()) 0)
  (check-equal? (sum-of-rooms rooms) 1514)

  (check-equal? (sum-of-rooms (file->lines "day04a.txt")) 158835)

  (check-equal? (decode-name "ab-cde-" 27) "bc def")

  (check-equal? (assoc "northpole object storage"
                       (decode-valid-rooms (file->lines "day04a.txt")))
                '("northpole object storage" 993))

  (displayln "done"))
