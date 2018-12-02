#!/usr/bin/env elvish

counts2 = 0
counts3 = 0

cat input.txt | each [l]{
  freqs = [&]
  each [c]{
    if (not (has-key $freqs $c)) { freqs[$c] = 0 }
    freqs[$c] = (+ $freqs[$c] 1)
  } $l
  if (has-value $freqs 2) { counts2 = (+ $counts2 1) }
  if (has-value $freqs 3) { counts3 = (+ $counts3 1) }
}
* $counts2 $counts3
