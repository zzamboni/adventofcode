#!/usr/bin/env elvish
use re
use github.com/zzamboni/elvish-modules/util

# Read input
claims = [(cat input.txt | each [l]{
      n x y w h = (re:find '^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)' $l)[groups][1 2 3 4 5][text]
      put [&id=$n &x=$x &y=$y &w=$w &h=$h]
})]
#pprint $claims

# Compute size of the board
maxx = (util:max (each [c]{ + $c[x] $c[w] } $claims))
maxy = (util:max (each [c]{ + $c[y] $c[h] } $claims))

#put $maxx $maxy

# Create a byte map for the board
map = [(repeat (* $maxx $maxy) 0)]

# Function to set values for a whole patch
fn mark [c]{
  #pprint $c
  x y w h = $c[x y w h]
  range $w | each [i]{
    range $h | each [j]{
      pos = (+ (* (+ $y $j -1) $maxx) $x $i)
      map[$pos] = (+ $map[$pos] 1)
    }
  }
}

# Check if a claim has any overlaps. Need $map to have been populated already
fn overlaps [c]{
  x y w h = $c[x y w h]
  and (
    range $w | each [i]{
      range $h | each [j]{
        == $map[(+ (* (+ $y $j -1) $maxx) $x $i)] 1
      }
  })
}

each [c]{ mark $c } $claims

count [(util:select [v]{ > $v 1 } $map)]

pprint (util:select $overlaps~ $claims)
