#!/usr/bin/env elvish
use github.com/zzamboni/elvish-modules/util

# Read the data and find the edges of the populated square
file = input.txt
if (not-eq $args []) { file = $args[0] }
points = [(cat $file | each [l]{
      put [(splits ", " $l)]
})]

npoints = (count $points)
minx = (util:min $@points &with=(util:partial $take~ 1))[0]
miny = (util:min $@points &with=(util:partial $drop~ 1))[1]
maxx = (util:max $@points &with=(util:partial $take~ 1))[0]
maxy = (util:max $@points &with=(util:partial $drop~ 1))[1]

echo npoints: $npoints minx: $minx miny: $miny maxx: $maxx maxy: $maxy

# Create arrays to store the points by their X and Y coordinates, for easy finding
points-by-x = []
points-by-y = []

sort -n -t, -k 1 $file | each [l]{
  p = [(splits ", " $l)]
  if (or (eq $points-by-x []) (not-eq $points-by-x[-1][0][0] $p[0])) {
    points-by-x = [$@points-by-x [$p]]
  } else {
    points-by-x[-1] = [(explode $points-by-x[-1]) $p]
  }
}
sort -n -t, -k 2 $file | each [l]{
  p = [(splits ", " $l)]
  if (or (eq $points-by-y []) (not-eq $points-by-y[-1][0][1] $p[1])) {
    points-by-y = [$@points-by-y [$p]]
  } else {
    points-by-y[-1] = [(explode $points-by-y[-1]) $p]
  }
}

# echo "points-by-x:"
# each [xrow]{ print "[ "
#   each [p]{ print $p " " } $xrow
#   echo "]"
# } $points-by-x
# echo "points-by-y:"
# each [yrow]{ print "[ "
#   each [p]{ print $p " " } $yrow
#   echo "]"
# } $points-by-y

fn abs [n]{
  if (< $n 0) { - $n } else { + $n }
}

fn trunc [n]{
  splits '.' $n | take 1
}

fn mannhattan-distance [p1 p2]{
  + (abs (- $p2[0] $p1[0])) (abs (- $p2[1] $p1[1]))
}

# Find the rows in list closest to p, according to elem (0=x 1=y).  If
# p matches exactly the coordinates of one row, it returns all points
# in that row. If it does not match exactly any of the rows, it
# returns the two closest to it (before and after).
fn point-search [p list elem &start=0 &end=-1]{
  if (eq $end -1) { end = (- (count $list) 1) }
  #echo start: $start end: $end
  if (eq $start $end) {
    explode $list[$start]
  } else {
    pos = (trunc (/ (+ $start $end) 2))
    pos2 = (+ $pos 1)
    if (> $pos2 $end) {
      pos2 = $pos
    }
    ind = $list[$pos][0][$elem]
    p-ind = $p[$elem]
    #echo pos: $pos pos2: $pos2 ind: $ind p-ind: $p-ind
    if (eq $ind $p-ind) {
      explode $list[$pos]
    } elif (and (<= $ind $p-ind) (<= $p-ind $list[$pos2][0][$elem])) {
      explode $list[$pos]
      explode $list[$pos2]
    } elif (< $p-ind $ind) {
      point-search $p $list $elem &start=$start &end=$pos
    } else {
      point-search $p $list $elem &start=(+ $pos 1) &end=$end
    }
  }
}

closest = [&]

fn closest-point [point]{
  point-distances = [(each [p]{
        put [(mannhattan-distance $p $point) $p]
  } [(point-search $point $points-by-x 0) (point-search $point $points-by-y 1)])]
  min-distance-point = (util:min &with=[v]{ put $v[0] } $@point-distances)
  # If it's equally close to more than one point, no one wins, return a fake point
  howmany = 1
  each [p]{
    if (and (eq $min-distance-point[0] $p[0]) (not-eq $min-distance-point[1] $p[1])) {
      howmany = (+ $howmany 1)
  } } $point-distances
  if (eq 1 $howmany) {
    put $min-distance-point[1]
  } else {
    put [-1 -1]
  }
}

closest-count = [&]

range $minx (+ $maxx 1) | each [x]{
  range $miny (+ $maxy 1) | each [y]{
    print [$x $y] ""
    closest = (closest-point [$x $y])
    #echo "  - closest point:" $closest
    if (not (has-key $closest-count $closest)) {
      closest-count[$closest] = 1
    } else {
      closest-count[$closest] = (+ $closest-count[$closest] 1)
    }
  }
}
echo ""

# Now find the point with the maximum count which is NOT on an edge,
# because those extend forever.
max = [0 [-1 -1]]
keys $closest-count | each [p]{
  if (not (or (has-value [$minx $maxx] $p[0]) (has-value [$miny $maxy] $p[1]))) {
    if (> $closest-count[$p] $max[0]) {
      max = [$closest-count[$p] $p]
    }
  }
}

echo Counts: $closest-count
echo Maximum: $max
