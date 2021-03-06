#!/usr/bin/env elvish
use github.com/zzamboni/elvish-modules/util
use str

# Use first arg as input, by default read input.txt
input = (cat input.txt)
if (not-eq $args []) { input = $args[0] }

# Build list of [x X] for all letters
pairs = [(range (ord a) (+ (ord z) 1) | each [c]{ put [ (chr $c) (str:to-upper (chr $c)) ] })]

# Replace all consecutive xX/Xx letter pairs in a string
fn replace-letters [s]{
  each [p]{
    s = (replaces $p[1]$p[0] '' (replaces $p[0]$p[1] '' $s))
  } $pairs
  put $s
}

# Reduce a string
fn fully-react [s]{
  new = (replace-letters $s)
  while (not-eq $new $s) {
    s = $new
    new = (replace-letters $s)
  }
  put $new
}

# Part 1
new = (fully-react $input)
echo "Length of initial reduction: "(count $new)

# Part 2
shortest = (util:min &with=[v]{ count $v[1] } (
    each [p]{
      echo "  Testing improvement by removing "(joins / $p) >&2
      put [$p (fully-react (replaces $p[0] '' (replaces $p[1] '' $new)))]
  } $pairs))

echo "Shortest reduction by removing "(joins / $shortest[0])
echo "Length: "(count $shortest[1])
