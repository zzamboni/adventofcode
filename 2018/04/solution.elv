#!/usr/bin/env elvish

use re
use github.com/zzamboni/elvish-modules/util

guard = 0
timetable = [&]
startmin = 0

# Read data and build time table
cat input.txt | sort | each [l]{
  m g a w = (re:find '\[.*:(\d\d)\] (?:Guard #(\d+) begins shift|(falls asleep)|(wakes up))' $l)[groups][1 2 3 4][text]
  if (not-eq $g '') {
    guard = $g
    if (not (has-key $timetable $guard)) {
      timetable[$guard] = [(repeat 60 0)]
    }
  }
  if (not-eq $a '') { startmin = $m }
  if (not-eq $w '') {
    range $startmin $m | each [x]{ timetable[$guard][$x] = (+ $timetable[$guard][$x] 1) }
    startmin = 0
  }
}

# Strategy #1
maxguard maxmins = (explode (util:max &with=[v]{ put $v[1] } (
      keys $timetable | each [guard]{
        put [$guard (+ (explode $timetable[$guard]))]
})))

echo "Maximum minutes asleep: Guard #"$maxguard": "$maxmins

maxmin maxmins = (explode (util:max &with=[v]{ put $v[1] } (
      range 0 60 | each [min]{
        put [$min $timetable[$maxguard][$min]]
})))

echo "Most-asleep minute: "$maxmin" ("$maxmins" times)"
echo "Result Strategy #1:" (* $maxmin $maxguard)

# Strategy #2
maxtotalguard maxtotalmin maxtotalmins = (explode (util:max &with=[v]{ put $v[2] } (
      keys $timetable | each [guard]{
        range 0 60 | each [min]{
          put [$guard $min $timetable[$guard][$min]]
}})))

echo "Most-asleep minute: Guard #"$maxtotalguard" on minute "$maxtotalmin": "$maxtotalmins" times."
echo "Result Strategy #2:" (* $maxtotalguard $maxtotalmin)
