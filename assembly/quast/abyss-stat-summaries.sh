# by Amirhossein Afshinfard 
# afshinfard @ github / aafshinfard @ github
# aafshinfard@gmail.com

# incomplete

dir_pattern="assembly-k*/assembly-stats"

echo "" > abyss_n50_summary.txt
for d in ${dir_pattern} ; do     echo "$d">>abyss_n50_summary.txt; grep "scaffolds" ${d} | awk '{print $5"\t"$6}' >>abyss_n50_summary.txt; done
cat abyss_n50_summary.txt | awk '{if(NF<2) print "|"; else printf "\t"; printf "| %s", $(NF)} END {print " |"}'>abyss_n50_summary.tsv

sort -rnk4 abyss_n50_summary.tsv | head -n1


