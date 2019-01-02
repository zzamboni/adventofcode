#lang racket
(require srfi/13)

(define (recipe-vector n [r1 3] [r2 7])
  (let ([v (make-vector n 0)])
    (vector-set*! v 0 r1 1 r2)
    v))

(define (digits n)
  (let-values ([(res _ __)
                (for/fold ([d '()]
                           [q (quotient n 10)]
                           [r (remainder n 10)])
                          ([i (in-naturals)]
                           #:break (and (zero? q) (zero? r)))
                  (values (cons r d) (quotient q 10) (remainder q 10)))])
    (if (empty? res) '(0) res)))

(define (step! v n e1 e2)
  (let* ([s1 (vector-ref v e1)]
         [s2 (vector-ref v e2)]
         [r  (+ s1 s2)]
         [d  (list->vector (digits r))]
         [nl (+ n (vector-length d))])
    (vector-copy! v n d 0 (min (vector-length d) (- (vector-length v) n)))
    (values nl (modulo (+ e1 s1 1) nl) (modulo (+ e2 s2 1) nl))))

(define (recipe-seq n)
  (let* ([g (recipe-vector n)])
    (for/fold ([l 2] [e1 0] [e2 1])
              ([i (in-naturals)]
               #:break (>= l n))
      (step! g l e1 e2))
    g))

(define (recipe-seq-str n)
  (string-join (map number->string (vector->list (recipe-seq n))) ""))

(define (counter)
  (define n 0)
  (lambda ([d 10000000]) (set! n (+ d n)) n))

; Call game1 with the input as a number
(define (game1 n)
  (string-join (map number->string (vector->list (vector-take-right (recipe-seq (+ n 10)) 10))) ""))

; Call game2 with the input as a string (in case of leading zeros)
(define (game2 ns)
  (for/fold ([found #f])
            ([n (in-producer (counter))]
             #:break found)
    (printf "Trying sequence of length ~a~n" n)
    (let* ([s (recipe-seq-str n)])
      (string-contains s ns))))
