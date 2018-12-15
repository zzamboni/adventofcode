#lang racket
(require racket/match)

(define (new-ring n-players last-marble)
  (make-hash (list (cons 'marbles (make-vector (add1 last-marble) 0))
                   (cons 'current 0)
                   (cons 'len 1)
                   (cons 'scores (make-vector n-players 0))
                   ;(cons 'score-traces (make-vector n-players '()))
                   ;(cons 'dropped-marbles (make-vector n-players '()))
                   )))

(define (ring-pos ring delta)
  (modulo (+ (hash-ref ring 'current) delta) (hash-ref ring 'len)))
                            
(define (ring-current ring)
  (hash-ref ring 'current))

(define (ring-set-current! ring c)
  (hash-set! ring 'current c))

(define (ring-marbles ring)
  (hash-ref ring 'marbles))

(define (ring-scores ring)
  (hash-ref ring 'scores))

; Add v in the position after pos
(define (ring-add! ring pos v)
  (let ([marbles (hash-ref ring 'marbles)]
        [len (hash-ref ring 'len)])
    (vector-copy! marbles (+ pos 2) marbles (+ pos 1) len)
    (vector-set! marbles (+ pos 1) v)
    (hash-set! ring 'len (add1 len))))

; Drop (and return) the marble at position pos
(define (ring-drop! ring pos)
  (let* ([marbles (hash-ref ring 'marbles)]
         [dropped (vector-ref marbles pos)]
         [len (hash-ref ring 'len)])
    (vector-copy! marbles pos marbles (+ pos 1) len)
    (hash-set! ring 'len (sub1 len))
    dropped))

(define (ring-print ring)
  (printf "~a~n" (string-join (for/list ([m (ring-marbles ring)]
                                         [i (in-naturals)])
                                (if (= i (ring-current ring)) (format "(~a)" m) (format "~a" m))) " ")))

(define (play ring player marble [trace-player -1])
  (if (zero? (modulo marble 23))
      (let* ([dropped-pos (ring-pos ring -7)]
             [dropped-marble (ring-drop! ring dropped-pos)]
             [old-score (vector-ref (ring-scores ring) player)]
             [new-score (+ old-score marble dropped-marble)])
;        (when (or (= player trace-player) (= trace-player -2))
;          ;(printf "Score for player #~a. Old score: ~a    Added marble: ~a (23*~a)    Dropped marble: ~a    New score: ~a~n" player old-score marble (/ marble 23) dropped-marble new-score)
;          (printf "Dropped marble: ~a~n" dropped-marble)
;          )
        (vector-set! (ring-scores ring) player new-score)
;        (vector-set! (hash-ref ring 'score-traces) player (append (vector-ref (hash-ref ring 'score-traces) player) (list marble)))
;        (vector-set! (hash-ref ring 'dropped-marbles) player (append (vector-ref (hash-ref ring 'dropped-marbles) player) (list dropped-marble)))
        (ring-set-current! ring dropped-pos))
      (let ([newpos (ring-pos ring 1)])
        (ring-add! ring newpos marble)
        (ring-set-current! ring (add1 newpos)))))

(define (game n-players last-marble [trace-player -1])
  (let ([ring (new-ring n-players last-marble)])
    (for ([marble (in-range 1 (add1 last-marble))]
          [i (in-naturals)])
      (let ([player (modulo i n-players)])
        (play ring player marble trace-player)
        ;(ring-print ring)
        ))
;    (printf "Score traces:~n~a~n" (string-join (map (lambda (v) (format "~a" v)) (vector->list (hash-ref ring 'score-traces))) "\n"))
;    (printf "Dropped marbles:~n~a~n" (string-join (map (lambda (v) (format "~a" v)) (vector->list (hash-ref ring 'dropped-marbles))) "\n"))
    (printf "Max score for ~a players and ~a marbles: ~a~n" n-players last-marble (apply max (vector->list (ring-scores ring))))))

;(require profile)
;(profile-thunk (lambda () (game 416 71617)))

(time (game 416 71617))
(time (game 416 7161700))
