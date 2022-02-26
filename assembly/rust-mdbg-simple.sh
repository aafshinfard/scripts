#!/usr/bin/env bash


# example:
expr="example2"
reads=reads-0.00
t=8
mkdir ${expr}
memusg -o ${expr}/${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik example/${reads}.fq.gz ${expr}/${reads}.rust-mdbg.multik.t${t} $t > ${expr}/${reads}.rust-mdbg.multik.t${t}.logs 2>&1 &


reads=$1
t=$2


conda_env rust-env #activate rust-env conda env which contains rust-mdbg requirements.
target/release/rust-mdbg --version >> ${reads}.rust-mdbg.version

#memusg -o ${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik ${reads}.fq.gz "${reads}.rust-mdbg.multik.t${t}" $t

reads=HG002_HiFi
reads_full=/projects/btl/datasets/hsapiens/NA24385_HG002_PacBio_HiFi/HG002_PacBio_CCS_all.fa.gz

expr="paper-hpcg02-02"
t=8

mkdir ${expr}
target/release/rust-mdbg --version >> ${expr}/${reads}.rust-mdbg.version
# multi-k
memusg -o ${expr}/${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik ${reads}.fa.gz ${expr}/${reads}.rust-mdbg.multik.t${t} $t > ${expr}/${reads}.rust-mdbg.multik.t${t}.logs 2>&1 &
# or
memusg -o ${expr}/${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik ${reads_full} ${expr}/${reads}.rust-mdbg.multik.t${t} $t > ${expr}/${reads}.rust-mdbg.multik.t${t}.logs 2>&1 &


# custom k=21,l=14,d=0.003
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.memusg -t target/release/rust-mdbg -k 21 -l 14 --density 0.003 ${reads}.fq.gz --threads ${t} --prefix ${expr}/${reads}.rust-mdbg.custom.t${t}. --bf > ${expr}/${reads}.rust-mdbg.custom.t${t}.logs 2>&1 &
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.magic_simplify.memusg -t utils/magic_simplify ${expr}/${reads}.rust-mdbg.custom.t${t}. > ${expr}/${reads}.rust-mdbg.custom.t${t}.magic_simplify.logs 2>&1 &
# or
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.memusg -t target/release/rust-mdbg -k 21 -l 14 --density 0.003 ${reads_full} --threads ${t} --prefix ${expr}/${reads}.rust-mdbg.custom.t${t}. --bf > ${expr}/${reads}.rust-mdbg.custom.t${t}.logs 2>&1 &
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.magic_simplify.memusg -t utils/magic_simplify ${expr}/${reads}.rust-mdbg.custom.t${t}. > ${expr}/${reads}.rust-mdbg.custom.t${t}.magic_simplify.logs 2>&1 &


expr="t48-hpcg02"
t=48
mkdir ${expr}
# multi-k
memusg -o ${expr}/${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik ${reads}.fq.gz ${expr}/${reads}.rust-mdbg.multik.t${t} $t > ${expr}/${reads}.rust-mdbg.multik.t${t}.logs 2>&1 &
# auto
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.memusg -t target/release/rust-mdbg --bf --threads ${t} ${reads}.fq.gz -p ${expr}/${reads}.rust-mdbg.custom.t${t}. > ${expr}/${reads}.rust-mdbg.custom.t${t}.logs 2>&1 &




expr="paper-hpcg01"
t=8

mkdir ${expr}
# multi-k
memusg -o ${expr}/${reads}.rust-mdbg.multik.t${t}.memusg -t utils/multik ${reads} ${expr}/${reads}.rust-mdbg.multik.t${t} $t > ${expr}/${reads}.rust-mdbg.multik.t${t}.logs 2>&1 &
# custom k=21,l=14,d=0.003
memusg -o ${expr}/${reads}.rust-mdbg.custom.t${t}.memusg -t target/release/rust-mdbg --bf --threads ${t} -k 21 -l 14 -d 0.003 ${reads} -p ${expr}/${reads}.rust-mdbg.custom.t${t}. > ${expr}/${reads}.rust-mdbg.custom.t${t}.logs 2>&1 &
