#!/bin/zsh

for i in /Users/chunyu/Documents/archive/cangnan/2segmentation/02vowel/${1}/*.wav; do
  # for i in /Users/chunyu/Desktop/min-vowel/01segmentation/${1}/*.wav; do
  /Applications/Praat.app/Contents/MacOS/Praat --run getFormant.praat $i $1
done
# iconv -f utf-16be -t UTF-8 /Users/chunyu/Desktop/min-vowel/02analysis/data/xiamen/${1}.txt >temp_file &&
#   mv temp_file /Users/chunyu/Desktop/min-vowel/02analysis/data/xiamen/${1}.txt
