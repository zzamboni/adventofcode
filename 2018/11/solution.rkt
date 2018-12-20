#lang racket
(require memoize)

(module+ test
  (require rackunit))

; Basic power level computation
(define/memo (power-level serial x y)
  (let* ([rack-id (+ x 10)])
    (- (remainder (quotient (* (+ (* rack-id y) serial) rack-id) 100) 10) 5)))

; Whole matrix of power levels, as a vector
(define (all-power-levels serial [size 300])
  (for*/vector #:length (* size size) #:fill 0
    ([y (in-range size)]
     [x (in-range size)])
    ; Power levels are calculated on 1-based indices
    (power-level serial (add1 x) (add1 y))))

(module+ test
  (check-equal? (power-level 8 3 5) 4)
  (check-equal? (power-level 57 122 79) -5)
  (check-equal? (power-level 39 217 196) 0)
  (check-equal? (power-level 71 101 153) 4))

; Access values in a matrix stored as a flat vector
(define (matrix-ref matrix size x y)
  (if (or (< x 0) (< y 0)) 0
      (vector-ref matrix (+ (* size y) x))))

(define (print-matrix matrix size)
  (for* ([y (in-range size)]
         [x (in-range size)])
    (when (zero? x) (printf "~n"))
    (printf " ~a " (matrix-ref matrix size x y))))

; Summed-area table. See https://en.wikipedia.org/wiki/Summed-area_table
(define (summed-area value-matrix [size 300])
  (let ([matrix (make-vector (* size size) 0)])
    (for* ([y (in-range size)]
           [x (in-range size)])
      (vector-set! matrix (+ (* size y) x)
                   (+ (matrix-ref value-matrix size x y)
                      (matrix-ref matrix size x (sub1 y))
                      (matrix-ref matrix size (sub1 x) y)
                      (- (matrix-ref matrix size (sub1 x) (sub1 y))))))
    matrix))

(define (area-sum summed-area size x0 y0 x1 y1)
  (+ (matrix-ref summed-area size x0 y0)
     (matrix-ref summed-area size x1 y1)
     (- (matrix-ref summed-area size x1 y0))
     (- (matrix-ref summed-area size x0 y1))))

(module+ test
  ;; Example from Wikipedia page https://en.wikipedia.org/wiki/Summed-area_table#/media/File:Integral_image_application_example.svg
  (let* ([orig-matrix (vector 31 2 4 33 5 36 12 26 9 10 29 25 13 17 21 22 20 18 24 23 15 16 14 19 30 8 28 27 11 7 1 35 34 3 32 6)]
         [summed (summed-area orig-matrix 6)])
    (check-equal? summed '#(31 33 37 70 75 111 43 71 84 127 161 222 56 101 135 200 254 333 80 148 197 278 346 444 110 186 263 371 450 555 111 222 333 444 555 666))
    (check-equal? (area-sum summed 6 1 2 4 4) 111)))

; Brute force approach
(define/memo (old-square-total serial x y size)
  (apply +
         (for*/list ([cx (range x (+ x size))]
                     [cy (range y (+ y size))])
           (power-level serial cx cy))))

; Summed-area table method for computing the total for a square
(define (square-total serial mx my square-size #:size [size 300] #:matrix [given-matrix '()])
  (let ([matrix (if (empty? given-matrix) (summed-area (all-power-levels serial size) size) given-matrix)]
        [x (sub1 mx)] [y (sub1 my)])
    (area-sum matrix size (sub1 x) (sub1 y) (+ x square-size -1) (+ y square-size -1))))

(module+ test
  (check-equal? (old-square-total 18 33 45 3) 29)
  (check-equal? (old-square-total 42 21 61 3) 30)
  (check-equal? (square-total 18 33 45 3) 29)
  (check-equal? (square-total 42 21 61 3) 30))

(define (largest-square serial [square-size 3] #:size [size 300] #:matrix [given-matrix '()])
  (let ([matrix (if (empty? given-matrix) (summed-area (all-power-levels serial)) given-matrix)])
    (for*/fold ([max-x 0] [max-y 0] [max-score 0])
               ([x (in-range (- size square-size))]
                [y (in-range (- size square-size))])
      (let ([score (square-total serial x y square-size #:matrix matrix)])
        (if (> score max-score)
            (values x y score)
            (values max-x max-y max-score))))))

; First part
(define (first-part)
  (let-values ([(x y score) (largest-square 8979)])
    (printf "~a,~a~n" x y)))

(define (largest-size serial)
  (let ([matrix (summed-area (all-power-levels serial))])
    (for/fold ([max-x 0] [max-y 0] [max-size 1] [max-score 0])
              ([size (range 1 301)])
      (let-values ([(x y score) (largest-square serial size #:matrix matrix)])
        (if (> score max-score)
            (values x y size score)
            (values max-x max-y max-size max-score))))))

; Second part
(define (second-part)
  (let-values ([(x y size score) (largest-size 8979)])
    (printf "~a,~a,~a~n" x y size)))
