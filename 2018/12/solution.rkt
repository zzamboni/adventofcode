#lang racket
(require racket/format)

(module+ test
  (require rackunit))

(define (rule-string->binary s)
  (regexp-replaces s '([#rx"#" "1"] [#rx"\\." "0"])))

(module+ test
  (check-equal? (rule-string->binary "##..#..##.#....##.#..#.#.##.#.#.######..##.#.#.####.#..#...##...#....#....#.##.###..#..###...#...#..")
                "1100100110100001101001010110101011111100110101011110100100011000100001000010110111001001110001000100"))

(define (bool->rule d)
  (if d "#" "."))

(define (bool->binary d)
  (if d "1" "0"))

(define (bools->rule l)
  (string-join (map bool->rule l) ""))

(define (bools->binary l)
  (string-join (map bool->binary l) ""))

(define (rule-string->number s)
  (string->number (rule-string->binary s) 2))

(module+ test
  (check-equal? (rule-string->number "##..#..") 100)
  (check-equal? (rule-string->number "##..#..##.#....##.#..#.#.##.#.#.######..##.#.#.####.#..#...##...#....#....#.##.###..#..###...#...#..")
                998430483121465337193355320388))

(define (read-input file)
  (for/fold ([rules (make-vector 32 #f)]
             [initial-state ""])
            ([line (file->lines file)])
    (let ([m (regexp-match #px"(.....) => (.)" line)]
          [n (regexp-match #px"initial state: (.*)" line)])
      (when m
        (let ([index (rule-string->number (second m))]
              [value (string=? (third m) "#")])
          (vector-set! rules index value)))
      (if n (values rules (second n)) (values rules initial-state)))))

; str must be a 5-digit binary number, returns the new value (#t/#f) of the middle bit
(define (evolve-bit str rules)
  ;(printf "str: ~a~n" str)
  (let ([res (vector-ref rules (string->number str 2))])
    ;(printf "~a => ~a~n" str res)
    res))

; str must be a binary number, advances all the bits
(define (evolve-string str rules input-length [steps 1])
  (for/fold ([res str] [value 0])
            ([step (range steps)])
    (let* ([tmpstr (string-join (list "00" res "00") "")]
           [array-res (for/list ([i (range (- (string-length tmpstr) 4))])
                        (evolve-bit (substring tmpstr i (+ i 5)) rules))]
           [new-value (for/sum ([c array-res] [n (in-naturals)] #:when c) (- n (* 2 input-length)))]
           [binary-res (bools->binary array-res)])
      (printf "~a: ~a (~a): ~a~n" step new-value (- new-value value) (bools->rule array-res))
      (values binary-res new-value))))

;(define-values [rules input] (read-input "test-input.txt"))
;(define binstr (string-join (list "00000000000" (rule-string->binary input) "00000000000") ""))

(define (solve [file "input.txt"] #:steps [steps 20])
  (let-values ([(rules input) (read-input file)])
    (let* ([input-l (string-length input)]
           [padded-input (rule-string->binary (~a input #:width (* input-l 5) #:pad-string "." #:align 'center))])
      (let-values ([(result value) (evolve-string padded-input rules input-l steps)])
        value))))

; After step 160, every step adds 33 to the count, so we don't have to compute all the way by hand.
(define (solve2 [steps 50000000000])
  (+ (solve #:steps 160) (* (- steps 160) 33)))