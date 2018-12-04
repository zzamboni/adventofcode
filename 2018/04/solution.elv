#!/usr/bin/env elvish

use re

guard = 0
timetable = [&]
startmin = 0

# Read data and build time table
cat input.txt | sort | each [l]{
  # Add with 0 to convert to a number (remove trailing zero)
  m = (+ (re:find '\[.*:(\d\d)\]' $l)[groups][1][text] 0)
  if (re:match 'begins shift' $l) {
    guard = (re:find 'Guard #(\d+) ' $l)[groups][1][text]
    if (not (has-key $timetable $guard)) {
      timetable[$guard] = [(repeat 60 0)]
    }
  }
  if (re:match 'falls asleep' $l) {
    startmin = $m
  }
  if (re:match 'wakes up' $l) {
    range $startmin $m | each [x]{ timetable[$guard][$x] = (+ $timetable[$guard][$x] 1) }
    startmin = 0
  }
}

# Strategy #1
maxguard = 0
maxmins = 0
keys $timetable | each [guard]{
  mins = (+ (explode $timetable[$guard]))
  if (> $mins $maxmins) { maxguard = $guard; maxmins = $mins }
}

echo "Maximum minutes asleep: Guard #"$maxguard": "$maxmins

maxmin = 0
maxmins = 0
range 0 60 | each [min]{
  if (> $timetable[$maxguard][$min] $maxmins) { maxmin = $min; maxmins = $timetable[$maxguard][$min] }
}
echo "Most-asleep minute: "$maxmin" ("$maxmins" times)"
echo "Result:" (* $maxmin $maxguard)

# Strategy #2
maxtotalguard = 0
maxtotalmin = 0
maxtotalmins = 0
keys $timetable | each [guard]{
  range 0 60 | each [min]{
    if (> $timetable[$guard][$min] $maxtotalmins) {
      maxtotalmins = $timetable[$guard][$min]
      maxtotalmin = $min
      maxtotalguard = $guard
    }
  }
}
echo "Most-asleep minute: Guard #"$maxtotalguard" on minute "$maxtotalmin": "$maxtotalmins" times."
echo "Result:" (* $maxtotalguard $maxtotalmin)
