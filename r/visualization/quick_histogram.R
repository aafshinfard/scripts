library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

df_temp = read.table(colClasses = c("numeric"),"/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/abyss2.5/na12878_2x151-2/assembly-k90-kc2-B200G/troubleshoot/liba-6.dist.dot_n-values.dot")

df_temp = read.table(colClasses = c("numeric"),"/projects/btl_scratch/aafshinfard/projects/stash/datasets/celegans/nanosim/from_ref/chr1_1mb/celegans_chr1-1mb_max100kb_n5k_perfect_aligned_reads.read_length")
df_temp = read.table(colClasses = c("numeric"),"/projects/btl_scratch/aafshinfard/projects/stash/datasets/celegans/nanosim/from_ref/chr1/celegans_chr1_max500k_n75k_perfect_aligned_reads.read_length")
df_temp = read.table(colClasses = c("numeric"),"/projects/btl_scratch/aafshinfard/projects/stash/datasets/celegans/nanosim/from_ref/genome/celegans_genome_max500kb_n500k_perfect_aligned_reads.read_length")



x_lab="Read length"
title="C.elegans - Chromosomes 1 to 6 - simulated perfect reads"


head(df_temp)
class(df_temp[1,1])



ggplot(df_temp, aes(x=V1)) + geom_bar()

ggplot(df_temp, aes(x=V1)) + 
  geom_histogram(binwidth = 1000)+
  xlim(500,45000)+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15, colour = "black"),
    legend.title = element_text(size=15),
    legend.text = element_text(size=15)
  ) + xlab(x_lab) + ggtitle(title)
