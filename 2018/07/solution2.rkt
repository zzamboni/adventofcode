#lang racket
(require racket/set)

(define workers 5)
(define (read-input) (map (lambda (l) (cdr (regexp-match #rx"Step (.) must be finished before step (.) can begin" l))) (file->lines "input.txt")))
(define (alphabet-hash) (make-hash (build-list 26 (lambda (x) (cons (string (integer->char (+ x (char->integer #\A)))) 0)))))

(define matrix (alphabet-hash))
(define letters (sort (hash-keys matrix) string<?))

(define (letter-value l)
  (+ 61 (- (char->integer (string-ref l 0)) (char->integer #\A))))

(define (set-coord from to val)
  (hash-set! (hash-ref matrix from) to val))

(define (get-coord from to)
  (hash-ref (hash-ref matrix from) to))

(define (print-matrix)
  (printf "   ")
  (for ([col letters])
    (printf " ~a " col))
  (printf "~n")
  (for ([row letters])
    (printf " ~a " row)
    (for ([col letters])
      (printf " ~a " (get-coord row col)))
    (printf "~n")))

(define (init-matrix pairs)
  (for ([l letters])
    (hash-set! matrix l (alphabet-hash))
    (set-coord l l (letter-value l)))
  (for ([p pairs])
    (let ([from (first p)]
          [to (second p)])
      (set-coord from to (letter-value from)))))

(define (can-be-done step done-steps)
  (for/and ([l letters] #:unless (string=? step l))
    (and (zero? (get-coord l step)) (not (member step done-steps)))))

(define (advance-step step)
  (for/fold ([done #t])
            ([l letters])
    (let* ([cur (get-coord step l)]
           [new (if (> cur 0) (sub1 cur) cur)])
      (set-coord step l new)
      (and done (zero? new)))))

(define (advance-steps steps)
  (for/fold ([done '()])
            ([s steps])
    (if (advance-step s) (append done (list s)) done)))

(define (take-at-most list n)
  (take list (if (> n (length list)) (length list) n)))

(define (doable-steps n done-steps)
  (take-at-most (sort (filter (lambda (l) (can-be-done l done-steps)) letters) string<?) n))

(define (list-subtract a b)
  (set->list (set-subtract (list->set a) (list->set b))))

(define (solve-puzzle)
  (let-values ([(steps prog nsteps)
                (for/fold ([done '()]
                           [progress '()]
                           [nsteps 0])
                          ([i (in-naturals)]
                           #:break (or (and (empty? (doable-steps workers done)) (empty? progress))))
                  (let* ([next-steps (sort (list-subtract (doable-steps workers done) progress) string<?)]
                         [in-progress (take-at-most (append progress next-steps) workers)]
                         [finished (advance-steps in-progress)])
                    (printf "i=~a done=~a progress=~a next-steps=~a in-progress=~a finished=~a~n" i done progress next-steps in-progress finished)
                    ;(print-matrix)
                    (values 
                     (append done finished)
                     (list-subtract in-progress finished)
                     i)))])
    (printf "~a ~a ~a" (string-join steps "") (string-length (string-join steps "")) (add1 nsteps))))

(init-matrix (read-input))