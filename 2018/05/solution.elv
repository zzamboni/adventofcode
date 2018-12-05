#!/usr/bin/env elvish
use github.com/zzamboni/elvish-modules/util

input = (cat input.txt)
if (not-eq $args []) { input = $args[0] }

new = ''

pairs = [(range (ord a) (+ (ord z) 1) | each [c]{ put [ (chr $c) (chr (- $c 32)) ] })]

fn replace-letters [s]{
  each [p]{
    s = (replaces $p[1]$p[0] '' (replaces (joins '' $p) '' $s))
  } $pairs
  put $s
}

fn fully-react [s]{
  while $true {
    new = (replace-letters $s)
    if (eq $new $s) { break }
    s = $new
  }
  put $new
}

new = (fully-react $input)
echo "Result of initial reduction: "$new
echo "Length: "(count $new)

improved = [&]
each [p]{
  echo "  Testing by removing "(joins / $p)
  improved[$p] = (fully-react (replaces $p[0] '' (replaces $p[1] '' $input)))
} $pairs

shortest = (util:min &with=[v]{ count $improved[$v] } $@pairs)

echo "Shortest reduction by removing "(joins / $shortest)
echo "Length: "(count $improved[$shortest])
