#lang racket
(define pairs (map (lambda (l) (cdr (regexp-match #rx"Step (.) must be finished before step (.) can begin" l))) (file->lines "input.txt")))
(define (alphabet-hash) (make-hash (build-list 26 (lambda (x) (cons (string (integer->char (+ x (char->integer #\A)))) #f)))))

(define matrix (alphabet-hash))
(define letters (hash-keys matrix))
(define done (alphabet-hash))

(for ([l (hash-keys matrix)])
  (hash-set! matrix l (alphabet-hash)))

(define (set-coord from to val)
  (hash-set! (hash-ref matrix from) to val))

(define (get-coord from to)
  (hash-ref (hash-ref matrix from) to))

(define (done-step step)
  (hash-ref done step))

(for ([p pairs])
  (let ([from (first p)]
        [to (second p)])
    (set-coord from to #t)))

(define (can-be-done step)
  (for/and ([l letters])
    (and (not (get-coord l step)) (not (done-step step)))))

(define (do-step step)
  (for ([l letters])
    (set-coord step l #f))
  (hash-set! done step #t))

(define (doable-steps)
  (sort (filter can-be-done letters) string<?))

(define (solve-puzzle)
  (string-join (for/fold ([steps '()])
                         ([i (in-naturals)]
                          #:break (empty? (doable-steps)))
                 (let ([next (first (doable-steps))])
                   (do-step next)
                   (append steps (list next)))) ""))
