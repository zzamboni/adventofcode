#lang racket
(require racket/match)

(match-define (list n-players last-marble)
  (map string->number
       (cdr (regexp-match #px"(\\d+) players; last marble is worth (\\d+) points"
                          (file->string "input.txt")))))

(define ring (make-hash '((marbles . (0)) (current . 0))))

(define player-scores (make-vector n-players 0))

(define (ring-pos ring delta)
  (modulo (+ (hash-ref ring 'current) delta) (length (hash-ref ring 'marbles))))
                            
(define (ring-current ring)
  (hash-ref ring 'current))

(define (ring-set-current! ring c)
  (hash-set! ring 'current c))

(define (ring-marbles ring)
  (hash-ref ring 'marbles))

; Add v in the position after pos
(define (ring-add! ring pos v)
  (hash-update! ring 'marbles
                (lambda (m)
                  (append (take m (add1 pos))
                          (list v)
                          (drop m (add1 pos))))))

; Drop (and return) the marble at position pos
(define (ring-drop! ring pos)
  (let ([dropped (list-ref (ring-marbles ring) pos)])
    (hash-update! ring 'marbles
                  (lambda (m)
                    (append (take m pos)
                            (drop m (add1 pos)))))
    dropped))

(define (ring-print ring)
  (printf "~a~n" (string-join (for/list ([m (ring-marbles ring)]
                                         [i (in-naturals)])
                                (if (= i (ring-current ring)) (format "(~a)" m) (format "~a" m))) " ")))

(define (play player marble)
  (if (zero? (modulo marble 23))
      (let* ([dropped-pos (ring-pos ring -7)]
             [dropped-marble (ring-drop! ring dropped-pos)])
        (vector-set! player-scores player (+ (vector-ref player-scores player) marble dropped-marble))
        (ring-set-current! ring dropped-pos))
      (let ([newpos (ring-pos ring 1)])
        (ring-add! ring newpos marble)
        (ring-set-current! ring (add1 newpos)))))

(define (play-all)
  (for ([marble (in-range 1 (add1 last-marble))]
        [i (in-naturals)])
    (let ([player (modulo i n-players)])
      (play player marble)
      ;(ring-print ring)
      ))
  (printf "Max score: ~a~n" (apply max (vector->list player-scores))))
