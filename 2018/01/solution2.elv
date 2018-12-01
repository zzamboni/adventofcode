#!/usr/bin/env elvish
input = [(cat input.txt)]
freqs = [&]
freq = 0
while $true {
  each [v]{
    freq = (+ $freq $v)
    if (has-key $freqs $freq) {
      echo "First repeated: "$freq
      exit
    } else {
      freqs[$freq] = $true
    }
  } $input
}
