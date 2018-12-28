#lang racket
(require ansi-color)

(struct game (map carts width height) #:mutable #:transparent)

(define (sort-carts cl)
  (sort cl <= #:key first))

(define (remove-dead-carts carts)
  (filter-not last carts))

(define (crashed-carts? c)
  (check-duplicates c #:key first))

(define (crashed? g)
  (crashed-carts? (game-carts g)))

(define (all-crashes g)
  (let-values ([(crashes _)
                (for/fold ([crashes '()]
                           [seen '()])
                          ([c (game-carts g)])
                  (if (member (first c) seen)
                      (values (cons (first c) crashes) seen)
                      (values crashes (cons (first c) seen))))])
    crashes))

(define (color fg bg what)
  (parameterize ([background-color bg]
                 [foreground-color fg])
    (color-display what)))

(define (display-game g)
  (let ([carts (game-carts g)]
        [w (game-width g)]
        [h (game-height g)]
        [crashed-pos (all-crashes g)])
    (for ([c (game-map g)]
          [i (in-naturals)])
      (when (zero? (modulo i w)) (printf "~n"))
      (let ([cart-here (findf (lambda (c) (= (first c) i)) carts)])
        (if cart-here
            (if (member (first cart-here) crashed-pos)
                (color 'black 'red "X")
                (color 'black 'green (format "~a" (second cart-here))))
            (display (format "~a" c)))))
    (display (format "~n"))))

(define (x-y pos w)
  (let-values ([(y x) (quotient/remainder pos w)])
    (list x y)))

(define (read-map [file "input.txt"])
  (let* ([lines (file->lines file)]
         [height (length lines)]
         [width (string-length (first lines))]
         [chars (flatten (map string->list lines))])
    (let-values ([(carts map)
                  (for/fold ([carts '()]
                             [new-chars '()])
                            ([c chars]
                             [i (in-naturals)])
                    (let ([coord (x-y i width)])
                      (cond
                        [(member c '(#\^ #\v)) (values (cons (list i c 0 coord #f) carts) (cons #\| new-chars))]
                        [(member c '(#\< #\>)) (values (cons (list i c 0 coord #f) carts) (cons #\- new-chars))]
                        [else                  (values carts                           (cons c   new-chars))])))])
      (game (list->vector (reverse map)) (sort-carts carts) width height))))

(define (handle-intersection cart)
  (let ([dir (second cart)]
        [turn (third cart)])
    (cond
      [(eq? dir #\^) (vector-ref #(#\< #\^ #\>) turn)]
      [(eq? dir #\v) (vector-ref #(#\> #\v #\<) turn)]
      [(eq? dir #\<) (vector-ref #(#\v #\< #\^) turn)]
      [(eq? dir #\>) (vector-ref #(#\^ #\> #\v) turn)])))

(define (handle-turn cart segment)
  (let ([dir (second cart)])
    (cond
      [(eq? dir #\^) (cond
                       [(eq? segment #\/) #\>]
                       [(eq? segment #\\) #\<])]
      [(eq? dir #\v) (cond
                       [(eq? segment #\/) #\<]
                       [(eq? segment #\\) #\>])]
      [(eq? dir #\<) (cond
                       [(eq? segment #\/) #\v]
                       [(eq? segment #\\) #\^])]
      [(eq? dir #\>) (cond
                       [(eq? segment #\/) #\^]
                       [(eq? segment #\\) #\v])])))

; Return the new value for cart after a move
(define (move cart game)
  (let* ([w (game-width game)]
         [pos (first cart)]
         [symb (second cart)]
         [turn (third cart)]
         [dead (last cart)]
         [next-pos (+ pos (second (assoc symb (list '(#\> 1) '(#\< -1) (list #\^ (- w)) (list #\v w)))))]
         [next-coord (x-y next-pos w)]
         [next-segment (vector-ref (game-map game) next-pos)])
    (cond
      [(eq? next-segment #\|) (list next-pos symb turn next-coord dead)]
      [(eq? next-segment #\-) (list next-pos symb turn next-coord dead)]
      [(eq? next-segment #\/) (list next-pos (handle-turn cart next-segment) turn next-coord dead)]
      [(eq? next-segment #\\) (list next-pos (handle-turn cart next-segment) turn next-coord dead)]
      [(eq? next-segment #\+) (list next-pos (handle-intersection cart) (modulo (add1 turn) 3) next-coord dead)])))

(define (mark-dead-carts carts)
  (for/fold ([new-carts '()])
            ([cart carts])
    (if (> (length (filter (lambda (c) (and (not (last c)) (not (last cart)) (= (first c) (first cart)))) carts)) 1)
        (cons (list (first cart) (second cart) (third cart) (fourth cart) #t) new-carts)
        (cons cart new-carts))))

(define (tick g)
  (let* ([map (game-map g)]
         [w (game-width g)]
         [h (game-height g)]
         [old-carts (sort-carts (game-carts g))])
    (let ([new-carts (for/fold ([carts (list->vector old-carts)])
                               ([cart-no (range (length old-carts))])
                       (let ([cart (vector-ref carts cart-no)])
                         (vector-set! carts cart-no (move cart g))
                         (list->vector (mark-dead-carts (vector->list carts)))))])
      (game map (sort-carts (vector->list new-carts)) w h))))

(define (solve1 [file "input.txt"])
  (let-values ([(last-game crash-pos)
                (let ([game (read-map file)])
                  (for/fold ([g game]
                             [crash #f])
                            ([i (in-naturals)]
                             #:break crash)
                    (let ([new-game (tick g)])
                      (values new-game (crashed? new-game)))))])
    (printf "Crash position: ~a,~a" (first (fourth crash-pos)) (second (fourth crash-pos)))))

(define (solve2 [file "input.txt"])
  (let ([last-game
         (for/fold ([g (read-map file)])
                   ([i (in-naturals)]
                    #:break (= (length (game-carts g)) 1))
           (let* ([new-game (tick g)]
                  [new-carts (mark-dead-carts (game-carts new-game))]
                  [filtered-carts (sort-carts (remove-dead-carts new-carts))])
             ;(display-game new-game)
             (set-game-carts! new-game filtered-carts)
             new-game))])
    (let* ([final-game last-game]
           [pos (fourth (first (game-carts final-game)))])
      (display-game final-game)
      (printf "Carts: ~a~n" (game-carts final-game))
      (printf "Last cart: ~a,~a~n" (first pos) (second pos)))))
