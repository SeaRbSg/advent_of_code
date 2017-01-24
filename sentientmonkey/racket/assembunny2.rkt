#lang br/quicklang

(define (read-syntax path port)
  (define args (port->lines port))
  (define handle-datums (format-datums ''(handle ~a) args))
  (define module-datum `(module assembunny-mod "assembunny2.rkt"
                          ,@handle-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (assembunny-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (println program)
     (println cpu)))
(provide (rename-out [assembunny-module-begin #%module-begin]))

(define (handle [op #f] [x #f] [y #f])
  (when op
    (set! program (vector-append program (vector (list op x y))))))

(define-syntax-rule (while condition body ...)
                    (let loop ()
                      (when condition
                        body ...
                        (loop))))

(define cpu (vector 0 0 0 0))
(define pc 0)
(define program (vector))

(define (register-index register)
  (case register
    ['a 0]
    ['b 1]
    ['c 2]
    ['d 3]))

(define (set-register register value)
  (vector-set! cpu (register-index register) value))

(define (get-register register)
  (vector-ref cpu (register-index register)))

(define (inc x)
  (set-register x (add1 (get-register x))))

(define (dec x)
  (set-register x (sub1 (get-register x))))

(define (cpy x y)
  (set-register y x))

(define (jnz x y)
  (when (not (zero? (get-register x)))
    (set! pc (+ pc y))))

;; (define (run p)
;;   (while (< pc (vector-length p))
;;          (eval (vector-ref p pc) ns)
;;          (set! pc (add1 pc))))

(provide handle quote)
(provide cpy inc dec jnz set-register get-register register-index cpu pc program)
