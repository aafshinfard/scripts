#!/bin/bash

ref=${1:-"/projects/btl/datasets/ecoli/NC_000913.3_K12_MG1655.fa"}
file=${2:-"assembly-scaffolds.fa"}
dir_pattern=${3:-"assembly-k*/"}
t=${4:-48}
options=${5:-""}

root_dir=$(pwd)
for d in ${dir_pattern} ; do
  cd "$d"
  echo "Running quast at:"
  echo "    $d"
  /projects/btl/aafshinfard/tools/quast/quast-5.0.2/quast.py --no-icarus ${file} -r $ref $options --threads $t --output-dir quast-detailed-${file} > quast-detailed-${file}.out 2>quast-detailed-${file}.err # 2>&1 | tee quast-detailed-${file}.log
  cd ${root_dir}
done




