#lang rash
(require rash/demo/setup)

(define input #{cat input.txt |> port->lines |> sort _ string<? })

(for/list ([l input])
  (regexp-match #rx"\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:(\\d\\d)\\] Guard #(\\d+) begins shift" l))
