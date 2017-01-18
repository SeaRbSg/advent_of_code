#lang racket
(require syntax/strip-context)

(define (read in)
  (syntax->datum
   (read-syntax #f in)))

(define (lines->list lines)
  (map
    (lambda (l)
           (string-split l " "))
    lines))

(define (list->ast ll)
  (filter
    (lambda (l) (not (empty? l)))
    (map
      (lambda (l)
        (match l
          [(list "cpy" x y) `(cpy ,x ,y)]
          [(list "jnz" x y) `(jnz ,x ,y)]
          [(list "inc" x) `(inc ,x)]
          [(list "dec" x) `(dec ,x)]
          [_ '()]))
      ll)))

(define (lines->ast lines)
  (list->ast (lines->list lines)))

(define (read-syntax src in)
  (with-syntax ([str (lines->ast (port->list read-line in))])
     #'(module anything racket
         (println 'str))))

(provide read read-syntax)
