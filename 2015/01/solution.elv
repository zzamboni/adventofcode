#!/usr/bin/env elvish

input = (cat input.txt)
levels = [&"("=1 &")"=-1]
nums = [(each [c]{ put $levels[$c] } $input)]
+ $@nums

level = 0
range (count $nums) | each [i]{
  level = (+ $level $nums[$i])
  if (== $level -1) {
    echo "Basement at pos "(+ $i 1)
    exit
  }
}
