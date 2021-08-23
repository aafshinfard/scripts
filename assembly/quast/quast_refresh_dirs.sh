#!/bin/bash
# inputs:
# 1 - dir_pattern: pattern of directories to run quast-get-summaries and abyss-stat-summaries in (they may contain multiple or single assembly directories themselves)

dir_pattern=${1:"na24*S*"}

#### Rerun "abyss-stat-summaries" and "quast-get-summaries" for all directories that match the pattern

for d in ${dir_pattern} ; do  cd "$d"; echo "$d"; /projects/btl/aafshinfard/github/scripts/assembly/quast/abyss-stat-summaries.sh; /projects/btl/aafshinfard/github/scripts/assembly/quast/quast-get-summaries.sh; cd ..; done

