#lang racket

(require "myutils.rkt")

(define-match-expander n! (syntax-rules () [(_ v) (app string->number v)]))
(define-match-expander s! (syntax-rules () [(_ v) (app string->symbol v)]))

(define output #f)
(define bots #f)

(define (populate l)
  (set! bots (list->vector l))
  (set! output (make-list (length l) #f)))

(define debug #f)

(define (dprintf . args)
  (when debug
    (apply printf args)))

(define (bot n v)
  (dprintf "(bot ~a ~a)~n" n v)
  (vector-set! bots n ((vector-ref bots n) v)))

(define (out n v)
  (dprintf "(out ~a ~a)~n" n v)
  (set! output (list-set output n v)))

(define (check n l h)
  (when (and (= l 17) (= h 61))
    (printf ";; ***** bot ~a handles both 17 & 61~n" n)))

(define (delay n proc)
  (curry (lambda (a b)
           (dprintf ";; triggering bot ~a with ~a & ~a~n" n a b)
           (apply (lambda (l h) (check n l h) (proc l h))
                  (sort (list a b) <)))))

(define (parse input)
  (define x (hash 'bot bot
                  'output out))
  (define (func k) (hash-ref x k))

  (define-values (bots assigns)
   (partition (lambda (x) (procedure? (cdr x)))
              (for/list ([line (in-list (parse-lines input))])
                (match line
                  [(pregexp #px"value (\\d+) goes to bot (\\d+)"
                            (list _ (n! from) (n! to)))
                   (cons from to)]
                  [(pregexp #px"bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)"
                            (list _ (n! from)
                                  (s! low-obj) (n! low-num)
                                  (s! high-obj) (n! high-num)))
                   (cons from (delay from (lambda (l h)
                                            (dprintf "WIRE: (~a ~a ~a)~n" low-obj low-num l)
                                            (dprintf "WIRE: (~a ~a ~a)~n" high-obj high-num l)
                                            ((func low-obj) low-num l)
                                            ((func high-obj) high-num h))))]
                  [else (list 'no line)]
                  ))))

  (values (map cdr (sort bots (lambda (a b) (< (car a) (car b))))) assigns))

(module+ test
  (require rackunit)

  (define input #<<END
value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2
END
    )

  (define (run in)
    (define-values (botsx assigns) (parse in))

    (populate botsx)

    (for ([assign (in-list assigns)])
      (bot (cdr assign) (car assign))))

  ;; (run input)

  (run (data-file 10))                  ; 10a
  (apply * (take output 3))             ; 10b
)
