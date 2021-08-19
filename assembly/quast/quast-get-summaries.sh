#!/bin/bash
# by Amirhossein Afshinfard 
# afshinfard @ github / aafshinfard @ github
# aafshinfard@gmail.com

# incomplete

#dir_pattern="2x*/*quast/"
dir_pattern="assembly-k*/*quast-*/"


#### Summarize in Vlad's style
# credits for quast-get-summary: @vnikolic
echo "" > quast_full_summary.txt
for d in ${dir_pattern} ; do     echo "$d">>quast_full_summary.txt; /projects/btl/aafshinfard/scripts/vnikolic/quast-get-summary "$d">>quast_full_summary.txt; done

#### Print in a tablular format
cat quast_full_summary.txt | awk '{if(NF<2) print "|"; else printf "\t"; printf "|%s", $(NF)} END {print "|"}'>quast_full_summary.tsv
rm quast_full_summary.txt
cat quast_full_summary.tsv

#### ABySS specific: split based on kc value
#grep "kc2" quast_full_summary.tsv >quast_kc2_summary.tsv
#grep "kc3" quast_full_summary.tsv >quast_kc3_summary.tsv
#grep "kc4" quast_full_summary.tsv >quast_kc4_summary.tsv
#grep "kc5" quast_full_summary.tsv >quast_kc4_summary.tsv
