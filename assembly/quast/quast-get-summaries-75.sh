#!/bin/bash
# by Amirhossein Afshinfard 
# afshinfard @ github / aafshinfard @ github
# aafshinfard@gmail.com

# Input:
dir_pattern=${1:-"assembly-k*/*quast*/"}

#### Summarize in Vlad's style
# credits for quast-get-summary: @vnikolic
echo "" > quast_full_summary-75.txt
for d in ${dir_pattern} ; do     echo "$d">>quast_full_summary-75.txt; ./quast-get-summary-75 "$d">>quast_full_summary-75.txt; done

#### Print in a tablular format
cat quast_full_summary-75.txt | awk '{if(NF<2) print "|"; else printf "\t"; printf "|%s", $(NF)} END {print "|"}'>quast_full_summary-75.tsv
rm quast_full_summary-75.txt
cat quast_full_summary-75.tsv

#### ABySS specific: split based on kc value
#grep "kc2" quast_full_summary.tsv >quast_kc2_summary.tsv
#grep "kc3" quast_full_summary.tsv >quast_kc3_summary.tsv
#grep "kc4" quast_full_summary.tsv >quast_kc4_summary.tsv
#grep "kc5" quast_full_summary.tsv >quast_kc4_summary.tsv
