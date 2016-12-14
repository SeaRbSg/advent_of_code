#lang racket

(require rackunit)

(struct santa (x y compass) #:transparent)

(define compass-new
  '(north east south west))

(define (santa-new)
  (santa 0 0 compass-new))

(define (santa-direction s)
  (car (santa-compass s)))

(check-equal? (santa-x (santa-new)) 0)
(check-equal? (santa-y (santa-new)) 0)
(check-equal? (santa-direction (santa-new)) 'north)

(define (santa-move s x y)
  (santa (+ (santa-x s) x) (+ (santa-y s) y) (santa-compass s)))

(check-equal? (santa-x (santa-move (santa-new) 1 0)) 1)
(check-equal? (santa-y (santa-move (santa-new) 0 1)) 1)

(define (santa-distance s)
  (+ (abs (santa-x s)) (abs (santa-y s))))

(check-equal? (santa-distance (santa 3 -2 compass-new)) 5)

(define (compass-turn-right c)
  (append (cdr c) (list (car c))))

(define (compass-turn-left c)
  (cons (last c) (drop-right c 1)))

(check-equal? (compass-turn-right compass-new) '(east south west north))
(check-equal? (compass-turn-left compass-new) '(west north east south))

(define (santa-turn-left s)
  (santa (santa-x s) (santa-y s) (compass-turn-left (santa-compass s))))

(define (santa-turn-right s)
  (santa (santa-x s) (santa-y s) (compass-turn-right (santa-compass s))))

(struct move (direction distance) #:transparent)

(define (string->move s)
  (let ([l (string->list s)])
    (move
      (string->symbol (string (car l)))
      (string->number (string (cadr l))))))

(check-equal? (move-direction (string->move "L1")) 'L)
(check-equal? (move-distance (string->move "L1")) 1)
(check-equal? (string->move "L1") (move 'L 1))

(define (apply-compass-move s m)
  (case (move-direction m)
    ['L (santa-turn-left s)]
    ['R (santa-turn-right s)]))

(check-equal? (santa-compass (apply-compass-move (santa-new) (string->move "L1")))
              '(west north east south))

(check-equal? (santa-compass (apply-compass-move (santa-new) (string->move "R1")))
              '(east south west north))

(define (apply-direction-move s m)
  (case (santa-direction s)
    ['north (santa-move s 0 (move-distance m))]
    ['east (santa-move s (move-distance m) 0)]
    ['south (santa-move s 0 (* -1 (move-distance m)))]
    ['west (santa-move s (* -1 (move-distance m)) 0)]))

(check-equal? (santa-y (apply-direction-move (santa-new) (string->move "L1"))) 1)

(define (apply-move s m)
  (apply-direction-move (apply-compass-move s m) m))

(check-equal? (apply-move (santa-new) (string->move "L1"))
              (santa -1 0 '(west north east south)))

(check-equal? (apply-move (santa-new) (string->move "R2"))
              (santa 2 0 '(east south west north)))

(define (apply-moves s l)
  (if (null? l)
    s
    (apply-moves (apply-move s (car l)) (cdr l))))

(define (string->moves s)
  (map string->move
       (string-split s ", ")))

(check-equal? (string->moves "R2, L3")
              (list (move 'R 2) (move 'L 3)))

(check-equal? (apply-moves (santa-new) (string->moves "R2, L3"))
              (santa 2 3 '(north east south west)))

(define (santa-navigate m)
  (santa-distance (apply-moves (santa-new) (string->moves m))))

(check-equal? (santa-navigate "R2, L3") 5)
(check-equal? (santa-navigate "R2, R2, R2") 2)
(check-equal? (santa-navigate "R5, L5, R5, R3") 12)

(apply-moves (santa-new) (string->moves "L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1"))

(santa-navigate "L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1")
