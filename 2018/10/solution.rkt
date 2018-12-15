#lang racket
(require plot)
;(plot-new-window? #t)

(define sky-points (map (lambda (l) (map string->number
                                     (cdr (regexp-match #px"position=\\<\\s*(-?\\d+),\\s*(-?\\d+)\\> velocity=\\<\\s*(-?\\d+),\\s*(-?\\d+)\\>" l))))
                      (file->lines "input.txt")))

(define bounds (list (apply min (map first sky-points)) (apply min (map second sky-points))
                     (apply max (map first sky-points)) (apply max (map second sky-points))))

(define (go #:start [start-step 0]
            #:increment [increment 1]
            #:x-min [x-min (- (first bounds) 1000)]
            #:y-min [y-min (- (second bounds) 1000)]
            #:x-max [x-max (+ (third bounds) 1000)]
            #:y-max [y-max (+ (fourth bounds) 1000)]
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