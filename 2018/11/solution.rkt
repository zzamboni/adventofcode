#lang racket
(require memoize)

(module+ test
  (require rackunit))

(define/memo (power-level serial x y)
  (let* ([rack-id (+ x 10)])
    (- (remainder (quotient (* (+ (* rack-id y) serial) rack-id) 100) 10) 5)))

(module+ test
  (check-equal? (power-level 8 3 5) 4)
  (check-equal? (power-level 57 122 79) -5)
  (check-equal? (power-level 39 217 196) 0)
  (check-equal? (power-level 71 101 153) 4))

(define/memo (square-total serial x y size)
  (apply +
         (for*/list ([cx (range x (+ x size))]
                     [cy (range y (+ y size))])
           (power-level serial cx cy))))

(module+ test
  (check-equal? (square-total 18 33 45 3) 29)
  (check-equal? (square-total 42 21 61 3) 30))

(define (largest-square serial [size 3] [mx 300] [my 300])
  (for*/fold ([max-x 0] [max-y 0] [max-score 0])
             ([x (in-range (- mx size))]
              [y (in-range (- my size))])
    (let ([score (square-total serial x y size)])
      (if (> score max-score)
          (values x y score)
          (values max-x max-y max-score)))))

; First part
(largest-square 8979)

; Second part
(define (largest-size serial)
  (for/fold ([max-x 0] [max-y 0] [max-size 1] [max-score 0])
            ([size (range 1 300)])
    (let-values ([(x y score) (largest-square serial size)])
      (if (> score max-score)
          (values x y size score)
          (values max-x max-y max-size max-score)))))

          