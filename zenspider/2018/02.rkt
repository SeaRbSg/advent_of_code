#lang racket

(require "./myutils.rkt")

(define s1 '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))
(define s2 '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))

(define (one b) (if b 1 0))

(define (solve1 ss)
  (define letters (map (compose occur string->list) ss))
  letters
  )

(solve1 s1)

(newline)
(map (compose
      (curry group-by car)
      (curry map (λ (l) (cons (length l) (car l))))
      (curry group-by identity)
      string->list) s1)

(newline)

(define counts (for/list ([s (in-list s1)])
   (for/fold ([h (hash)])
             ([c (in-string s)])
     (hash-update h c add1 0))))

counts

(newline)

(define (occur/2 xs [hashfn hash])
  (define grouped (for/fold ([h (hashfn)])
                            ([x (in-list xs)])
                    (hash-update h x add1 0)))
  (for/fold ([h (hashfn)])
            ([(v k) (in-hash grouped)])
    (hash-update h k (curry cons v) (thunk '()))))

(for/list ([s (in-list s1)])
  (occur/2 (string->list s)))

;; solve1 :: [String] -> Int
;; solve1 ss = count (==2) letters * count (==3) letters
;;   where letters    = fmap occur ss
;;         count p xs = sum $ fmap (one . any p) xs
;;
;; problem1 :: String -> String
;; problem1 = show . solve1 . lines
;;
;; solve2 :: [String] -> [String]
;; solve2 ss = [ c
;;             | a <- ss
;;             , b <- ss
;;             , a > b
;;             , let c = fmap fst . filter (uncurry (==)) $ a `zip` b
;;             , length a - length c == 1
;;             ]
;;
;; solve3 :: [String] -> [String]
;; solve3 ss = do a <- ss
;;                b <- ss
;;                guard $ a > b
;;                let c = fmap fst . filter (uncurry (==)) $ a `zip` b
;;                guard $ length a - length c == 1
;;                pure c
;;
;; problem2 :: String -> String
;; problem2 = head . solve2 . lines
;;
;; problem3 :: String -> String
;; problem3 = head . solve3 . lines
;;
;; main :: IO ()
;; main = minteract [problem1, problem2, problem3]
