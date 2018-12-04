#lang rash
(require rash/demo/setup)
cat input.txt |> port->lines =map= string->number |> apply +
