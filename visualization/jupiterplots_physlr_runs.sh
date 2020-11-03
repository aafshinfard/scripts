############################################## Global variables
individual_NA=NA12878
individual_na=na12878
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}
ng=75
maxScaff=-1
setting_name=maxScaff-1_ng75
ref=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/grch38_no_Y_chromosome.fa
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}




#####################################################


################## [ stlfr
# Variable
tech=stlfr
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/

### Baseline
tool=baseline
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### ARKS
tool=arks
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Physlr
tool=physlr
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Rainbowplots
#
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/rainbowplots
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/hiveplot ./
#
tool=baseline
prefix1=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir1=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix1}
#
tool=arks
prefix2=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir2=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix2}
#
tool=physlr
prefix3=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir3=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix3}


./hiveplot name=base.arks prefix1=${dir1} prefix2=${dir2}
./hiveplot name=base.physlr prefix1=${dir1} prefix2=${dir3}
./hiveplot name=arks.physlr prefix1=${dir2} prefix2=${dir3}


################## stlfr ]

################## [ pempet
# Variable
tech=pempet
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/

### Baseline
tool=baseline
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### ARKS
tool=arks
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Physlr
tool=physlr
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft


### Rainbowplots
#
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/rainbowplots
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/hiveplot ./
#
tool=baseline
prefix1=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir1=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix1}
#
tool=arks
prefix2=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir2=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix2}
#
tool=physlr
prefix3=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir3=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix3}


./hiveplot name=base.arks prefix1=${dir1} prefix2=${dir2}
./hiveplot name=base.physlr prefix1=${dir1} prefix2=${dir3}
./hiveplot name=arks.physlr prefix1=${dir2} prefix2=${dir3}


################## pempet ]


################## [ pacbio
# Variable
tech=pacbio
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/

### Baseline
tool=baseline
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### ARKS
tool=arks
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Physlr
tool=physlr
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Rainbowplots
#
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/rainbowplots
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/hiveplot ./
#
tool=baseline
prefix1=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir1=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix1}
#
tool=arks
prefix2=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir2=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix2}
#
tool=physlr
prefix3=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir3=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix3}


./hiveplot name=base.arks prefix1=${dir1} prefix2=${dir2}
./hiveplot name=base.physlr prefix1=${dir1} prefix2=${dir3}
./hiveplot name=arks.physlr prefix1=${dir2} prefix2=${dir3}


################## pacbio ]


################## [ ont
# Variable
tech=ont
mkdir /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/

### Baseline
tool=baseline
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### ARKS
tool=arks
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Physlr
tool=physlr
#
name=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
draft=/projects/btl_scratch/aafshinfard/projects/physlr/publication/drafts/${individual_NA}/${tool}/${individual_na}.${tech}.${tool}.fa
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/jupiterplotcodes/jupiter ./
./jupiter ng=$ng maxScaff=$maxScaff t=16 name=$name ref=$ref fa=$draft

### Rainbowplots
#
dir=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/rainbowplots
mkdir $dir 
cd $dir
rm -rf *
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/*/ ./
ln -s /projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/rainbowplotscodes/hiveplot ./
#
tool=baseline
prefix1=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir1=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix1}
#
tool=arks
prefix2=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir2=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix2}
#
tool=physlr
prefix3=ref.VS.${individual_na}.${tech}.${tool}.${setting_name}
dir3=/projects/btl_scratch/aafshinfard/projects/physlr/publication/figures/jupiterplots/${individual_na}/${setting_name}/${tech}/${tool}/${prefix3}


./hiveplot name=base.arks prefix1=${dir1} prefix2=${dir2}
./hiveplot name=base.physlr prefix1=${dir1} prefix2=${dir3}
./hiveplot name=arks.physlr prefix1=${dir2} prefix2=${dir3}



################## ont ]
