#!/usr/bin/env elvish
input = [(cat input.txt)]
freqs = [&]
freq = 0
while (not (has-key $freqs $freq)) {
  freqs[$freq] = $true
  v @input = $@input
  if (eq $input []) { input = [(cat input.txt)] }
  freq = (+ $freq $v)
}
echo "First repeated: "$freq
