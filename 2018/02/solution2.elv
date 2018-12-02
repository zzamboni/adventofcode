#!/usr/bin/env elvish

input = [(cat input.txt)]

each [x]{
  each [y]{
    diff = 0
    common = ""
    range (count $x) | each [i]{
      if (not-eq $x[$i] $y[$i]) {
        diff = (+ $diff 1)
      } else {
        common = $common$x[$i]
      }
    }
    if (eq $diff 1) {
      echo $x "<->" $y
      echo Common: $common
      exit
    }
  } $input
} $input
