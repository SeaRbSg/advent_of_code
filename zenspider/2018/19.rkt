#lang racket/base

(require racket/match
         racket/string
         racket/list
         racket/function
         "../myutils.rkt")

;; switch setters to static compilation = ~2-4 ms    (~0.75s -> ~0.72s)
;; mutable structs                      = ~150-200ms (~0.70s -> ~0.55s)
;; reg struct -> vector                 = ~0ms (possibly -5ms)
;; inst struct -> vector                = ~0ms (~0.59s)
;; currying reg-[gs]etter               = ~150ms ? (~0.45s)
;; removing `in` from new ops           = ~0ms (~0.45s)

(define (inst op a b) (vector op a b))
(define (inst-op i) (vector-ref i 0))
(define (inst-a  i) (vector-ref i 1))
(define (inst-b  i) (vector-ref i 2))

(define (new/r) (vector 0 0 0 0 0 0 0))

(define ((reg-getter n) r) (vector-ref r n))
(define reg-r0 (reg-getter 0))          ; for tests
(define reg-pc (reg-getter 6))          ; for exec/go

(define (reg-set-pc r v) (vector-set! r 6 v) r)

(define ((reg-setter n) r v) (vector-set! r n v) r)

(define (b->i b) (if b 1 0))
(define gti (compose b->i >))
(define eqi (compose b->i =))

(define (parse s/f)
  ;; fn a b c #t #t ->
  ;; (let ([known slot getters] ...)
  ;;   (λ (r in) (setter r (fn (get-a) (get-b)))))

  (define (wrap/inst fn a b c a/reg b/reg)
    (define setter (reg-setter c))
    (define wrapped-fn
      (match (cons a/reg b/reg)
        ['(#t . #t) (let ([ga (reg-getter a)]
                          [gb (reg-getter b)])
                      (λ (r) (setter r (fn (ga r) (gb r)))))]
        ['(#t . #f) (let ([ga (reg-getter a)])
                      (λ (r) (setter r (fn (ga r) b))))]
        ['(#f . #t) (let ([gb (reg-getter b)])
                      (λ (r) (setter r (fn a      (gb r)))))]
        ['(#t . no) (let ([ga (reg-getter a)])
                      (λ (r) (setter r     (ga r))))]
        ['(#f . no)   (λ (r) (setter r     a))]))
    (inst wrapped-fn a b))

  (define-values (slot rest) (split-at (parse-lines-of-atoms s/f) 1))

  (define insts
    (for/vector ([line (in-list rest)])
      (match line
        [`(addr ,a ,b ,c) (wrap/inst + a b c #t #t)]
        [`(addi ,a ,b ,c) (wrap/inst + a b c #t #f)]
        [`(mulr ,a ,b ,c) (wrap/inst * a b c #t #t)]
        [`(muli ,a ,b ,c) (wrap/inst * a b c #t #f)]
        [`(banr ,a ,b ,c) (wrap/inst bitwise-and a b c #t #t)]
        [`(bani ,a ,b ,c) (wrap/inst bitwise-and a b c #t #f)]
        [`(borr ,a ,b ,c) (wrap/inst bitwise-ior a b c #t #t)]
        [`(bori ,a ,b ,c) (wrap/inst bitwise-ior a b c #t #f)]
        [`(setr ,a ,b ,c) (wrap/inst 'meh a b c #t 'no)]
        [`(seti ,a ,b ,c) (wrap/inst 'meh a b c #f 'no)]
        [`(gtir ,a ,b ,c) (wrap/inst gti a b c #f #t)]
        [`(gtri ,a ,b ,c) (wrap/inst gti a b c #t #f)]
        [`(gtrr ,a ,b ,c) (wrap/inst gti a b c #t #t)]
        [`(eqir ,a ,b ,c) (wrap/inst eqi a b c #f #t)]
        [`(eqri ,a ,b ,c) (wrap/inst eqi a b c #t #f)]
        [`(eqrr ,a ,b ,c) (wrap/inst eqi a b c #t #t)]
        [_ (list 'fucked line)])))
  (values (cadar slot) insts))

(define (exec reg slot insts)
  (define maxI         (vector-length insts))
  (define get-slot     (reg-getter slot))
  (define (updatePC r) (reg-set-pc r (add1 (get-slot r))))
  (define updateSlot   (reg-setter slot))
  (define (go r)
    (define pc (reg-pc r))
    (if (< pc maxI)
        (let* ([i  (vector-ref insts pc)] ; get instruction via PC
               [r1 (updateSlot r pc)]     ; copy PC to <slot>
               [r2 ((inst-op i) r1)]      ; execute instruction
               [r3 (updatePC r2)])        ; copy <slot> + 1 to PC
          (go r3))
        r))
  (go reg))

(define (problem1 s)
  (let-values ([(slot insts) (parse s)])
    (exec (new/r) slot insts)))

(define (problem2 s)
  (let-values ([(slot insts) (parse s)])
    (exec ((reg-setter 0) (new/r) 1) slot insts)))

(define test/file (string-join
                   '("#ip 0"
                     "seti 5 0 1"
                     "seti 6 0 2"
                     "addi 0 1 0"
                     "addr 1 2 3"
                     "setr 1 0 0"
                     "seti 8 0 4"
                     "seti 9 0 5")
                   "\n"))

(module+ main
  (define arg (or
               (let ([args (vector->list (current-command-line-arguments))])
                 (if (empty? args)
                     #f
                     (car args)))
               test/file))
  (time (problem1 arg))
  #;(time (problem2 arg))
  )

(module+ mainx

  (require profile)
  (profile-thunk (thunk (problem1 "19.txt"))
                 #:order 'total
                 #:use-errortrace? #t))

(module+ test
  (require rackunit)

  (define r  (vector 3 2 1 0 0 0 0))
  (define i1 (inst 42 0 1))
  (define i3 (inst 42 0 3))

  (check-equal? ((reg-getter 0) r) 3)
  (check-equal? ((reg-setter 1) r 9) (vector 3 9 1 0 0 0 0))

  (check-equal? (reg-r0 (problem1 test/file)) 6)

  #;(check-equal? (reg-r0 (problem1 "19.txt")) 1248)
  42
  )

(module+ test
  (displayln 'done))
