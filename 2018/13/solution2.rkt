#lang racket
(require ansi-color)

(struct game (map carts width height) #:mutable #:transparent)
(struct cart (pos dir turn dead)
  #:mutable #:transparent)

(define (vector-first v)
  (vector-ref v 0))

(define (sort-carts cl)
  (vector-sort cl <= #:key cart-pos))

(define (cart-alive cart)
  (not (cart-dead cart)))

(define (live-carts carts)
  (vector-filter cart-alive carts))

(define (color fg bg what)
  (parameterize ([background-color bg]
                 [foreground-color fg])
    (color-display what)))

(define (carts-here carts pos)
  (vector-filter (lambda (c) (= (cart-pos c) pos)) carts))

(define (display-game g)
  (let ([carts (game-carts g)]
        [w (game-width g)]
        [h (game-height g)])
    (for ([c (game-map g)]
          [i (in-naturals)])
      (when (zero? (modulo i w)) (printf "~n"))
      (let ([carts-here (carts-here (live-carts carts) i)])
        (if (zero? (vector-length carts-here))
            (display (format "~a" c))
            (if (> (vector-length carts-here) 1)
                (color 'black 'red "X")
                (color 'black 'green (format "~a" (cart-dir (vector-first carts-here))))))))
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
                  (for/fold ([carts #()]
                             [new-chars '()])
                            ([c chars]
                             [i (in-naturals)])
                    (cond
                      [(member c '(#\^ #\v)) (values (vector-append carts (vector (cart i c 0 #f))) (cons #\| new-chars))]
                      [(member c '(#\< #\>)) (values (vector-append carts (vector (cart i c 0 #f))) (cons #\- new-chars))]
                      [else                  (values carts                     (cons c   new-chars))]))])
      (game (list->vector (reverse map)) (sort-carts carts) width height))))

; Return the new direction of a cart at an intersection
(define (handle-intersection cart)
  (let ([dir (cart-dir cart)]
        [turn (cart-turn cart)]
        [turns (hash #\^ #(#\< #\^ #\>)
                     #\v #(#\> #\v #\<)
                     #\< #(#\v #\< #\^)
                     #\> #(#\^ #\> #\v))])
    (vector-ref (hash-ref turns dir) turn)))

(define (handle-turn cart segment)
  (let ([dir (cart-dir cart)]
        [turns (hash '(#\^ #\/) #\> '(#\^ #\\) #\<
                     '(#\v #\/) #\< '(#\v #\\) #\>
                     '(#\< #\/) #\v '(#\< #\\) #\^
                     '(#\> #\/) #\^ '(#\> #\\) #\v)])
    (hash-ref turns (list dir segment))))

; Return the new value for cart after a move
(define (move this-cart game)
  (let* ([w (game-width game)]
         [pos  (cart-pos this-cart)]
         [symb (cart-dir this-cart)]
         [turn (cart-turn this-cart)]
         [dead (cart-dead this-cart)]
         [increments (hash #\> 1     #\< -1
                           #\^ (- w) #\v w)]
         [next-pos (+ pos (hash-ref increments symb))]
         [next-segment (vector-ref (game-map game) next-pos)])
    (cond
      [(eq?    next-segment #\+)        (cart next-pos (handle-intersection this-cart) (modulo (add1 turn) 3) dead)]
      [(member next-segment '(#\/ #\\)) (cart next-pos (handle-turn this-cart next-segment) turn dead)]
      [else                             (cart next-pos symb turn dead)])))

(define (move-cart game carts ncarts i)
  ;  (printf "Step #~a:~n" i)
  (let* ([cart (vector-ref carts (modulo i ncarts))]
         [new-cart (move cart game)]
         [new-pos (cart-pos new-cart)])
    ;    (printf "Cart: ~a   New cart: ~a~n" cart new-cart)
    (vector-set! carts (modulo i ncarts) new-cart)
    (let ([carts-here (carts-here (live-carts carts) new-pos)])
      ;      (display-game game)
      (when (> (vector-length carts-here) 1)
        (printf "Crash at ~a~n" (x-y (cart-pos (vector-first carts-here)) (game-width game)))
        (for ([c carts])
          (when (= (cart-pos c) new-pos)
            (set-cart-dead! c #t)))))))

(define (solve game)
  (let* ([carts (game-carts game)]
         [ncarts (vector-length carts)])
    ;    (display-game game)
    (for ([i (in-naturals)]
          #:when (cart-alive (vector-ref carts (modulo i ncarts))))
      #:final (= (vector-length (live-carts carts)) 1)
      (if (= (vector-length (live-carts carts)) 1)
          (printf "Last cart: ~a (~a)~n" (live-carts carts)
                    (x-y (cart-pos (vector-first (live-carts carts))) (game-width game)))
          (move-cart game carts ncarts i)))))

(define (play [file "input.txt"])
  (let ([g (read-map file)])
    (solve g)))
