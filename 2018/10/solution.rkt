#lang racket
(require plot)
;(plot-new-window? #t)

(define sky-points (map (lambda (l) (map string->number
                                         (cdr (regexp-match #px"position=\\<\\s*(-?\\d+),\\s*(-?\\d+)\\> velocity=\\<\\s*(-?\\d+),\\s*(-?\\d+)\\>" l))))
                        (file->lines "input.txt")))

(define (bounds [ps sky-points])
  (list (apply min (map first ps)) (apply min (map second ps))
        (apply max (map first ps)) (apply max (map second ps))))

(define (bounds-size b)
  (list (- (third b) (first b)) (- (fourth b) (second b))))

; First solution, through visual exploration (this is how I solved it initially)
(define (go #:start [start-step 0]
            #:increment [increment 1]
            #:x-min [x-min (- (first (bounds)) 1000)]
            #:y-min [y-min (- (second (bounds)) 1000)]
            #:x-max [x-max (+ (third (bounds)) 1000)]
            #:y-max [y-max (+ (fourth (bounds)) 1000)]
            #:width [width 1200]
            #:height [height 400]
            #:sleep [sleep-time 0.1]
            )
  (for/list ([step (in-naturals start-step)]
             #:when (zero? (modulo step increment)))
    (display (plot (points (map vector (map (lambda (p) (+ (first p) (* step (third p)))) sky-points)
                                (map (lambda (p) (- (+ (second p) (* step (fourth p))))) sky-points)))
                   #:title (format "Step: ~a" step)
                   #:x-min x-min #:y-min y-min
                   #:x-max x-max #:y-max y-max
                   #:width width #:height height))
    (sleep sleep-time)))

; Part 1 & 2 solution: (go #:start 10418 #:x-min 180 #:x-max 250 #:y-min -180 #:y-max -140 #:sleep 10)

; Second solution, through computation, finds the position where the points are closer together, and only plots that one.
(define (go2 #:start [start-step 0])
  (let-values ([(message last-bounds last-step _)
                (for/fold ([ps sky-points]
                           [min-bounds (bounds sky-points)]
                           [last-step 0]
                           [break #f])
                          ([step (in-naturals start-step)]
                           #:break break)
                  (let* ([new-ps (map (lambda (p) (list (+ (first p) (* step (third p)))
                                                        (- (+ (second p) (* step (fourth p))))
                                                        (third p) (fourth p))) sky-points)]
                         [new-bounds (bounds new-ps)]
                         [size (bounds-size min-bounds)]
                         [new-size (bounds-size new-bounds)]
                         ; Termination condition: if the new bounds are larger than the previous ones, the previous step was the solution
                         [will-break (and (> (first new-size) (first size)) (> (second new-size) (second size)))])
                    (if will-break
                        (values ps new-bounds (sub1 step) will-break)
                        (values new-ps new-bounds step will-break))))])
    (printf "Last step: ~a~n" last-step)
    (display (plot (points (map vector (map first message) (map second message)))
                   #:title (format "Step: ~a" last-step)
                   #:width 600 #:height 200
                   #:x-min (sub1 (first last-bounds))
                   #:y-min (sub1 (second last-bounds))
                   #:x-max (add1 (third last-bounds))
                   #:y-max (add1 (fourth last-bounds))))))
