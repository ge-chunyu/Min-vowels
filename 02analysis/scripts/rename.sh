#!/bin/zsh


for i in /Users/chunyu/Documents/archive/cangnan/2segmentation/02vowel/${1}/${2}/ ; do
  awk -F',' -v p="$i" -v s="${1}R${2:1}_" -v f="${1}-${2}-" 'system("mv " p s $3 ".wav " p f $4 ".wav" )' ~/Desktop/min-vowel/cn-filename.csv
  awk -F',' -v p="$i" -v s="${1}R${2:1}_" -v f="${1}-${2}-" 'system("mv " p s $3 ".TextGrid " p f $4 ".TextGrid" )' ~/Desktop/min-vowel/cn-filename.csv
done

# for i in */ ; do
#   for j in $i*/; do
#     # awk -F',' -v p="$j" '{ print p $1 " " p substr(p, 0, 3) "-" substr(p, 5, 2) "-" $2 }' test.csv
#     awk -F',' -v p="$j" 'system("mv " p $1 " " p substr(p, 0, 3) "-" substr(p, 5, 2) "-" $2)' test.csv
#   done
# done
#
