#lang racket

(require "../2016/myutils.rkt")

(struct cpu (pc count a b c d e f g h) #:transparent #:mutable)

(define (parse-lines-of-symbols-or-ints in)
  (for/list ([line (parse-lines in)])
    (for/list ([token (in-list (string-split line))])
      (or (string->number token)
          (string->symbol token)))))

(define getter
  (hash 'a cpu-a))

(define (get name)
  (hash-ref getter name)
  )

(define (update m k v)
42
  )

(define (run m input)
  (define program
   (for/vector ([inst (parse-lines-of-symbols-or-ints input)])
     ;; when /set (\S+) (\S+)/ then # set X Y: sets register X to value Y
     ;;   x, y = $1.to_v, $2.to_v
     ;;   if y.is_a? Integer then
     ;;     ->() { register[x] = y;            step }
     ;;   else
     ;;     ->() { register[x] = register[y];  step }
     ;;   end

     (match inst
       [`(set a ,(and y (? integer?)))
        (λ () (cpu (add1 (cpu-pc m)) (cpu-count m)
                   y (cpu-b m) (cpu-c m) (cpu-d m)
                   (cpu-e m) (cpu-f m) (cpu-g m) (cpu-h m)))]
       [`(set a ,y)
        (define yy (if (integer? y) y (get y)))
        (λ () (cpu (add1 (cpu-pc m)) (cpu-count m)
                   y (cpu-b m) (cpu-c m) (cpu-d m)
                   (cpu-e m) (cpu-f m) (cpu-g m) (cpu-h m)))]
       [`(set b ,(and y (? integer?)))
        (λ () (cpu (add1 (cpu-pc m)) (cpu-count m)
                   (cpu-a m) y (cpu-c m) (cpu-d m) (cpu-e m)
                   (cpu-f m) (cpu-g m) (cpu-h m)))]
       [`(set b ,y)
        (λ () (cpu (add1 (cpu-pc m)) (cpu-count m)
                   (cpu-a m) ((get y) m) (cpu-c m) (cpu-d m) (cpu-e m)
                   (cpu-f m) (cpu-g m) (cpu-h m)))]
       [_ #f]
       )
     ))
  ((vector-ref program (cpu-pc m)))
  )

(define (problem-23a input)
  #f)

(module+ test
  (require rackunit))

(module+ test
  (define new-cpu (cpu 0 0 0 0 0 0 0 0 0 0))

  (check-equal? (cpu-a new-cpu) 0)
  (check-equal? (run new-cpu "set a 42")
                (cpu 1 0 42 0 0 0 0 0 0 0))

  ;;       def test_sub
  ;;         cpu = Problem23a.new
  ;;
  ;;         assert_equal  0, cpu.register[:a]
  ;;         cpu.run "sub a -42"
  ;;         assert_equal 42, cpu.register[:a]
  ;;         assert_equal 1, cpu.pc
  ;;       end

  ;;       def test_mul
  ;;         cpu = Problem23a.new
  ;;         cpu.register[:a] = 2
  ;;
  ;;         cpu.run "mul a 21"
  ;;         assert_equal 42, cpu.register[:a]
  ;;         assert_equal 1, cpu.pc
  ;;         assert_equal 1, cpu.count
  ;;       end

  ;;       def test_jnz_true
  ;;         cpu = Problem23a.new
  ;;         cpu.register[:a] = 1
  ;;
  ;;         cpu.run "jnz a 42"
  ;;         assert_equal 42, cpu.pc
  ;;       end

  ;;       def test_jnz_true_const
  ;;         cpu = Problem23a.new
  ;;         cpu.register[:a] = 1
  ;;
  ;;         cpu.run "jnz 1 42"
  ;;         assert_equal 42, cpu.pc
  ;;       end

  ;;       def test_jnz_false
  ;;         cpu = Problem23a.new
  ;;         cpu.register[:a] = 0 # redundant
  ;;
  ;;         cpu.run "jnz a 42"
  ;;         assert_equal 1, cpu.pc
  ;;       end

  (check-equal? (problem-23a "123") 3))

(define (problem-23b input)
  #f)

(module+ test
  (check-equal? (problem-23b "123") 6))

(module+ test
  (displayln 'done))

(module+ main
  (define input (parse-file (data-file 23)))
  (problem-23a input)
  (problem-23b input))

;; require "prime"
;;
;; class String
;;   def to_v
;;     Integer(self) rescue self.to_sym
;;   end
;; end
;;
;; class Problem23a
;;   attr_accessor :register, :program, :pc, :count
;;
;;   def initialize
;;     self.register = Hash.new { |h,k| raise "Unknown register: #{k}" }
;;     [:a, :b, :c, :d, :e, :f, :g, :h].each do |name|
;;       register[name] = 0
;;     end
;;
;;     self.pc = 0
;;     self.count = 0
;;   end
;;
;;   def parse input
;;     self.program = input.lines.map { |line|
;;       case line
;;       when /set (\S+) (\S+)/ then # set X Y: sets register X to value Y
;;         x, y = $1.to_v, $2.to_v
;;         if y.is_a? Integer then
;;           ->() { register[x] = y;            step }
;;         else
;;           ->() { register[x] = register[y];  step }
;;         end
;;       when /sub (\S+) (\S+)/ then # sub X Y: subtracts register X by value Y
;;         x, y = $1.to_v, $2.to_v
;;
;;         if y.is_a? Integer then
;;           ->() { register[x] -= y;           step }
;;         else
;;           ->() { register[x] -= register[y]; step }
;;         end
;;       when /mul (\S+) (\S+)/ then # mul X Y: multiplies register X by value Y
;;         x, y = $1.to_v, $2.to_v
;;
;;         if y.is_a? Integer then
;;           ->() { register[x] *= y;           count; step }
;;         else
;;           ->() { register[x] *= register[y]; count; step }
;;         end
;;       when /jnz (\S+) (\S+)/ then # jnz X Y: pc += value Y, if the value X != 0
;;         x, y = $1.to_v, $2.to_v
;;
;;         if x.is_a? Integer then
;;           if x != 0 then
;;             ->() { step y }
;;           else
;;             ->() { step 1 }
;;           end
;;         else
;;           if x.is_a? Integer then
;;             ->() { step (x.zero?           ? 1 : y) }
;;           else
;;             ->() { step (register[x].zero? ? 1 : y) }
;;           end
;;         end
;;       else
;;         warn "unparsed: #{line.chomp}"
;;       end
;;     }
;;   end
;;
;;   def step n = 1
;;     self.pc += n
;;   end
;;
;;   def count
;;     self.count += 1
;;   end
;;
;;   def run input
;;     parse input
;;
;;     old_pc = self.pc
;;     inst = program[pc]
;;     begin
;;       inst.call
;;       old_pc = self.pc
;;     end while inst = program[old_pc]
;;
;;     self
;;   end
;; end
;;
;; class Problem23b < Problem23a
;;   def initialize
;;     super
;;     register[:a] = 1
;;   end
;;
;;   def run
;;     b = 57 * 100 + 100_000
;;     c = b + 17_000
;;     h = 0
;;
;;     (b..c).step(17).each do |bb|
;;       h += 1 unless bb.prime?
;;     end
;;
;;     h
;;   end
;; end
;;
;; if __FILE__ == $0 then
;;   if ARGV.empty? then
;;     require "minitest/autorun"
;;
;;
;;       def test_b
;;         skip
;;       end
;;     end
;;   else
;;     input = ARGF.read.chomp
;;     p Problem23a.new.run(input).count
;;     p Problem23b.new.run
;;   end
;; end


;; #lang racket
;;
;; (require math/number-theory)
;;
;; (define-namespace-anchor na)
;; (current-namespace (namespace-anchor->namespace na))
;;
;; (define procSymbol (compose string->symbol
;;                             (curry string-append "proc_")
;;                             number->string))
;; (define vlistrev (compose reverse vector->list))
;; (define-values
;;   (       a b c d e f g h)
;;   (values 0 0 0 0 0 0 0 0))
;; (define mulcounter 0)
;; (set! mulcounter 0)
;; (define data (for/vector ([op (in-port)]
;;                           [ic (in-naturals)])
;;                (let ([arg1 (read)]
;;                      [arg2 (read)])
;;                  (case op
;;                    [(set) (list 'set! arg1 arg2)]
;;                    [(jnz) (list op arg1 (+ ic arg2))]
;;                    [(mul) (list 'set!
;;                                 arg1
;;                                 (list 'begin
;;                                       (list 'set!
;;                                             'mulcounter
;;                                             (list 'add1 'mulcounter))
;;                                       (list '* arg1 arg2)))]
;;                    [(sub) (list 'set! arg1 (if (and (number? arg2) (= arg2 -1))
;;                                                (list add1 arg1)
;;                                                (list '- arg1 arg2)))]
;;                    [else  (error "UNKNOW OPERATION")]))))
;;
;; (define jumps (vector-filter (lambda (x) (symbol=? (car x) 'jnz)) data))
;;
;; (define (evalproclist name body)
;;   (list 'define
;;         name
;;         (append (list 'lambda '())
;;                 (if (empty? body)
;;                     '((void))
;;                     body))))
;;
;; (define (genifelse l r)
;;   (cond [(empty? l) r]
;;         [(symbol=? (caar l) 'jnz)
;;          (genifelse (cdr l)
;;                     (if (number? (cadar l))
;;                         (list (list (procSymbol (caddar l))))
;;                         (list (list 'if
;;                                     (list 'not (list '= (cadar l) 0))
;;                                     (list (procSymbol (caddar l)))
;;                                     (append '(begin) r)))))]
;;         [else (genifelse (cdr l) (cons (car l) r))]))
;;
;; (eval (evalproclist 'proc_0
;;                     (genifelse (vlistrev (vector-take data 4)) '())))
;;
;; (for ([e jumps])
;;   (let* ([id (third e)]
;;          [name (procSymbol id)]
;;          [body (vlistrev (vector-drop data id))])
;;     (eval (evalproclist name (genifelse body '())))))
;;
;; ;; part 1
;; (eval '(proc_0))
;; (displayln mulcounter)
;;
;; ;; part 2
;; (set!-values (a b c d e f g h)
;;              (values 1 0 0 0 0 0 0 0))
;; (eval '(set! proc_11 (lambda ()
;;                        (unless (prime? b) (set! h (add1 h)))
;;                        (proc_26))))
;; (eval '(proc_0))
;; (displayln h)
