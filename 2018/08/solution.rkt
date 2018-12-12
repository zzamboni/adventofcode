#lang racket
(define tree-numbers (map string->number (string-split (file->string "input.txt"))))

(define (extract-tree tlist pos)
  ;;(printf "Expanding ~a starting at ~a~n" tlist pos)
  (let* ([tsublist (drop tlist pos)]
         [n-subtrees (first tsublist)]
         [n-metadata (second tsublist)])
    (let-values ([(subtrees lastpos)
                  (for/fold ([-subtrees '()]
                             [-lastpos 2])
                            ([n (in-range n-subtrees)])
                    (let-values ([(tree endpos) (extract-tree tsublist -lastpos)])
                      (values (append -subtrees (list tree)) (+ -lastpos endpos)))
                    )])
      (let* ([meta (take (drop tsublist lastpos) n-metadata)]
             [treestruct (list n-subtrees n-metadata subtrees meta)])
        (values treestruct (+ lastpos n-metadata))))))

(define (parse-tree tlist)
  (let-values ([(tstruct n) (extract-tree tlist 0)])
    tstruct))

(define (traverse-apply tree func init)
  (match-let ([(list a b c d) tree])
    (for/fold ([new (func init tree)])
              ([subs c])
      ;;(printf "tree=~a new=~a subs=~a~n" tree new subs)
      (traverse-apply subs func new))))

(define (first-result) (traverse-apply (parse-tree tree-numbers) (lambda (v t) (apply + v (fourth t))) 0))

(define (node-value t)
  (match-let ([(list a b c d) t])
    (if (zero? a)
        (apply + d)
        (apply + (for/list ([ind d])
                   (if (<= 1 ind (length c)) (node-value (list-ref c (sub1 ind))) 0))))))

(define (second-result)
  (node-value (parse-tree tree-numbers)))
