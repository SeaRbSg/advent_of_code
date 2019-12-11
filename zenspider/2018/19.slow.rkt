#lang racket

(require "../myutils.rkt")

(struct reg  (r0 r1 r2 r3 r4 r5 pc) #:transparent)
(struct inst (op a b c) #:transparent)
(struct slot (n) #:transparent)

(define (new/r) (reg 0 0 0 0 0 0 0))

(define (reg-set-pc r v) (struct-copy reg r [pc v]))
(define (reg-set-r0 r v) (struct-copy reg r [r0 v]))
(define (reg-set-r1 r v) (struct-copy reg r [r1 v]))
(define (reg-set-r2 r v) (struct-copy reg r [r2 v]))
(define (reg-set-r3 r v) (struct-copy reg r [r3 v]))
(define (reg-set-r4 r v) (struct-copy reg r [r4 v]))
(define (reg-set-r5 r v) (struct-copy reg r [r5 v]))

(define (reg-set r n v)
  (case n
    [(0) (reg-set-r0 r v)]
    [(1) (reg-set-r1 r v)]
    [(2) (reg-set-r2 r v)]
    [(3) (reg-set-r3 r v)]
    [(4) (reg-set-r4 r v)]
    [(5) (reg-set-r5 r v)]
    [else (raise-arguments-error 'reg-set "bad value" "r" r "n" n "v" v)]))

(define (reg-get-r0 r) (reg-r0 r))
(define (reg-get-r1 r) (reg-r1 r))
(define (reg-get-r2 r) (reg-r2 r))
(define (reg-get-r3 r) (reg-r3 r))
(define (reg-get-r4 r) (reg-r4 r))
(define (reg-get-r5 r) (reg-r5 r))

(define (reg-get r n)
  (case n
    [(0) (reg-get-r0 r)]
    [(1) (reg-get-r1 r)]
    [(2) (reg-get-r2 r)]
    [(3) (reg-get-r3 r)]
    [(4) (reg-get-r4 r)]
    [(5) (reg-get-r5 r)]
    [else (raise-arguments-error 'reg-get "bad value" "r" r "n" n)]))

(define ((op/rr fn) r in)
  (reg-set r (inst-c in) (fn (reg-get r (inst-a in)) (reg-get r (inst-b in)))))

(define ((op/ri fn) r in)
  (reg-set r (inst-c in) (fn (reg-get r (inst-a in)) (inst-b in))))

(define ((op/ir fn) r in)
  (reg-set r (inst-c in) (fn            (inst-a in)  (reg-get r (inst-b in)))))

(define ((op/r) r in)
  (reg-set r (inst-c in) (reg-get r (inst-a in))))

(define ((op/i) r in)
  (reg-set r (inst-c in) (inst-a in)))

(define (b->i b) (if b 1 0))

(define gti (compose b->i >))
(define eqi (compose b->i =))

(define addr (op/rr +))
(define addi (op/ri +))
(define mulr (op/rr *))
(define muli (op/ri *))
(define banr (op/rr bitwise-and))
(define bani (op/ri bitwise-and))
(define borr (op/rr bitwise-ior))
(define bori (op/ri bitwise-ior))
(define setr (op/r))
(define seti (op/i))
(define gtir (op/ir gti))
(define gtri (op/ri gti))
(define gtrr (op/rr gti))
(define eqir (op/ir eqi))
(define eqri (op/ri eqi))
(define eqrr (op/rr eqi))

(define (parse s/f)
  (define out
   (for/list ([line (parse-lines-of-atoms s/f)])
     (match line
       [`(|#ip| ,n)      (slot n)]
       [`(addr ,a ,b ,c) (inst addr a b c)]
       [`(addi ,a ,b ,c) (inst addi a b c)]
       [`(mulr ,a ,b ,c) (inst mulr a b c)]
       [`(muli ,a ,b ,c) (inst muli a b c)]
       [`(banr ,a ,b ,c) (inst banr a b c)]
       [`(bani ,a ,b ,c) (inst bani a b c)]
       [`(borr ,a ,b ,c) (inst borr a b c)]
       [`(bori ,a ,b ,c) (inst bori a b c)]
       [`(setr ,a ,b ,c) (inst setr a b c)]
       [`(seti ,a ,b ,c) (inst seti a b c)]
       [`(gtir ,a ,b ,c) (inst gtir a b c)]
       [`(gtri ,a ,b ,c) (inst gtri a b c)]
       [`(gtrr ,a ,b ,c) (inst gtrr a b c)]
       [`(eqir ,a ,b ,c) (inst eqir a b c)]
       [`(eqri ,a ,b ,c) (inst eqri a b c)]
       [`(eqrr ,a ,b ,c) (inst eqrr a b c)]
       [_ (list 'fucked line)])))
  (values (car out) (list->vector (cdr out))))

(define (exec reg slot insts)
  (define maxI (vector-length insts))
  (define (updatePC r) (reg-set-pc r (add1 (reg-get r slot))))
  (define (go r)
    (define pc (reg-pc r))
    (if (>= pc maxI)
        r
        (begin
          (let* ([i (vector-ref insts pc)]
                 [r1 (reg-set r slot pc)]
                 [r2 ((inst-op i) r1 i)]
                 [r3 (updatePC r2)])
            (go r3)))))
  (go reg))

(define (problem1 s)
  (let-values ([(slot insts) (parse s)])
    (exec (new/r) (slot-n slot) insts)))

(define (problem2 s)
  (let-values ([(slot insts) (parse s)])
    (exec (reg-set (new/r) 0 1) (slot-n slot) insts)))

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

  (time (problem1 arg)))

(module+ test
  (require rackunit)

  (define r  (reg 0 3 2 1 0 0 0))
  (define i1 (inst 42 0 1 2))
  (define i3 (inst 42 0 3 2))

  (check-equal? (reg-get r 0) 3)
  (check-equal? (reg-set r 1 9) (reg 0 3 9 1 0 0 0))

  (check-equal? (addr r (inst 42 0 1 2)) (reg 0 3 2 5 0 0 0))
  (check-equal? (addi r (inst 42 0 3 2)) (reg 0 3 2 6 0 0 0))
  (check-equal? (mulr r (inst 42 0 1 2)) (reg 0 3 2 6 0 0 0))
  (check-equal? (muli r (inst 42 0 3 2)) (reg 0 3 2 9 0 0 0))
  (check-equal? (banr r (inst 42 0 1 2)) (reg 0 3 2 2 0 0 0)) ; 10 & 11 == 10
  (check-equal? (bani r (inst 42 0 3 2)) (reg 0 3 2 3 0 0 0)) ; 11 & 11 == 11
  (check-equal? (borr r (inst 42 0 1 2)) (reg 0 3 2 3 0 0 0)) ; 11 | 10 == 11
  (check-equal? (bori r (inst 42 0 3 2)) (reg 0 3 2 3 0 0 0)) ; 11 | 11 == 11

  (check-equal? (gtir r (inst 42 1 1 2)) (reg 0 3 2 0 0 0 0))
  (check-equal? (gtir r (inst 42 3 1 2)) (reg 0 3 2 1 0 0 0))
  (check-equal? (gtri r (inst 42 0 1 2)) (reg 0 3 2 1 0 0 0))
  (check-equal? (gtri r (inst 42 0 4 2)) (reg 0 3 2 0 0 0 0))
  (check-equal? (gtrr r (inst 42 0 1 2)) (reg 0 3 2 1 0 0 0))
  (check-equal? (gtrr r (inst 42 1 0 2)) (reg 0 3 2 0 0 0 0))

  (check-equal? (eqir r (inst 42 0 1 2)) (reg 0 3 2 0 0 0 0))
  (check-equal? (eqir r (inst 42 2 1 2)) (reg 0 3 2 1 0 0 0))
  (check-equal? (eqri r (inst 42 0 1 2)) (reg 0 3 2 0 0 0 0))
  (check-equal? (eqri r (inst 42 0 3 2)) (reg 0 3 2 1 0 0 0))
  (check-equal? (eqrr r (inst 42 0 1 2)) (reg 0 3 2 0 0 0 0))
  (check-equal? (eqrr r (inst 42 1 1 2)) (reg 0 3 2 1 0 0 0))

  (check-equal? (setr r (inst 42 0 1 2)) (reg 0 3 2 3 0 0 0))
  (check-equal? (seti r (inst 42 6 1 2)) (reg 0 3 2 6 0 0 0))

  (check-equal? (reg-r0 (problem1 test/file)) 6))

(module+ test
  (displayln 'done))
