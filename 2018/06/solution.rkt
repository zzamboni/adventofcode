#lang racket
(define points
  (map (lambda (s) (map string->number (string-split s #rx", ")))
       (file->lines "input.txt")))

(define min-x (sub1 (sub1 (apply min (map first points)))))
(define max-x (add1 (add1 (apply max (map first points)))))
(define min-y (sub1 (sub1 (apply min (map second points)))))
(define max-y (add1 (add1 (apply max (map second points)))))
(define size-x (- max-x min-x -1))
(define size-y (- max-y min-y -1))

(define (distance p1 p2)
  (+ (abs (- (first p1) (first p2))) (abs (- (second p1) (second p2)))))

(define (pos-in-vec x y)
  (+ (* size-x (- y min-y)) (- x min-x)))

(define (get-point vec x y)
  (vector-ref vec (pos-in-vec x y)))

(define (set-point! vec x y v)
  (vector-set! vec (pos-in-vec x y) v))

(define point-map (make-vector (* size-x size-y) '()))
(define disqualified (make-hash))

(define region-count
  (for*/fold ([region-size 0])
             ([x (in-range min-x (add1 max-x))]
              [y (in-range min-y (add1 max-y))])
    (let ([coord (list x y)])
      ;;(when (zero? (modulo x 100)) (printf "~a " coord))
      (let-values ([(closest-distance closest-points total-distance)
                    (for/fold 
                     ([min-d 1000]
                      [min-p '()]
                      [total-d 0])
                     ([p points])
                      (let* ([d (distance p coord)])
                        (values (if (< d min-d) d min-d)
                                (cond [(< d min-d) (list p)]
                                      [(= d min-d) (cons p min-p)]
                                      [else min-p])
                                (+ total-d d))))])
        (when (= 1 (length closest-points))
          (begin
            ;;(hash-update! closest-map (first closest-points) add1 0)
            (set-point! point-map x y (list closest-distance (first closest-points)))))
        (when (or (= x min-x) (= x max-x) (= y min-y) (= y max-y))
          (for ([disq closest-points])
            ;;(printf "Disqualifying ~a\n" disq)
            (hash-set! disqualified disq #t)))
        (if (< total-distance 10000) (add1 region-size) region-size)))))
  ;;(printf "~n")

(define closest-map (make-hash))
(for ([p points])
  ;;(printf "Counting points for ~a...\n" p)
  (hash-set! closest-map p (length (filter (lambda (v) (equal? v p)) (map (lambda (v) (if (empty? v) '() (second v))) (vector->list point-map))))))

(define all-counts (sort (map list (hash-keys closest-map) (hash-values closest-map)) > #:key second))
(define remaining-counts (filter (lambda (v) (not (hash-ref disqualified (car v) #f))) all-counts))

(printf "All counts: ~a~n" all-counts)
(printf "Disqualified points: ~a~n" disqualified)
(printf "Remaining counts: ~a~n" remaining-counts)
(printf "Region count: ~a~n" region-count)
