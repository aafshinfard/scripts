library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

df_temp = read.table(colClasses = c("numeric"),"/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/abyss2.5/na12878_2x151-2/assembly-k90-kc2-B200G/troubleshoot/liba-6.dist.dot_n-values.dot")

head(df_temp)
class(df_temp[1,1])



ggplot(df_temp, aes(x=V1)) + geom_bar()

ggplot(df_temp, aes(x=V1)) + 
  geom_histogram(binwidth = 1)+
  xlim(0,100)
