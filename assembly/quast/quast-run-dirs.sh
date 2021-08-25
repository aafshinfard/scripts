#!/bin/bash

dir_pattern=${1:-"assembly-k*/"}
t=${2:-48}

root_dir=$(pwd)
for d in ${dir_pattern} ; do
  cd "$d"
  echo "Running quast at:"
  echo "    $d"
  /projects/btl/aafshinfard/tools/quast/quast-5.0.2/quast.py --no-icarus ${file} -r $ref --large --threads $t --output-dir quast-detailed-${file} > quast-detailed-${file}.out 2>quast-detailed-${file}.err # 2>&1 | tee quast-detailed-${file}.log
  cd ${root_dir}
done




