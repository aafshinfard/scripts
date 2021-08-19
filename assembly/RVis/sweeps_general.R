# by Amirhossein Afshinfard 
# afshinfard @ github / aafshinfard @ github
# aafshinfard@gmail.com

##### Description:
# Input format: 1 file per experiment, each file looks like:
#| name_1 | NGA50_1 | NG50_1  | N50_1  | misassembly_1 | genome fraction_1 |   
#| name_2 | NGA50_2 | NG50_2  | N50_2  | misassembly_2 | genome fraction_2 |   
# ....
# name should be formatted as {.*-k[0-9]*-kc[0-9]-.*}, (like assembly-k100-kc2-B40G/quast-analysis)
# or you can change the script (the "reformat" function).
#
# These sort of files may be generated using: [TODO]
#   https://github.com/afshinfard/scripts/blob/main/assembly/quast/quast-get-summaries.sh

### Inputs:
##### E.Coli:

##### HSapiens:
species="hsapiens"
setwd("/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/figures/assembly_quality")

###________
exp1_n="na12878 k90_kc2 - N sweeps"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na12878_N_sweeps_k90_kc2/quast_full_summary.tsv";
exp2_n="na24143 k120_kc2 - N sweeps"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_N_sweeps_k120_kc2/quast_full_summary.tsv";
###________
exp1_n="na12878 k90_kc2_N15 - S sweeps"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na12878_k90_kc2_N15_S-sweeps/quast_full_summary.tsv";
###________
exp1_n="na24143 +konnector N=20"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_sweeps/quast_full_summary.tsv";
exp1_n="na24143 +konnector --extend N=20"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps/quast_full_summary.tsv";

exp2_n="na24143 +konnector N=10"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_sweeps_N10/quast_full_summary.tsv";
exp2_n="na24143 +konnector --extend N=10"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps_N10/quast_full_summary.tsv";

exp3_n="na24143 - abyss 2.3.1 & N[10-20] "
exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24143_2x250/quast_full_summary.tsv";
exp3_n="na24143 - abyss 2.3.1 & N[10-20] "
exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24143_2x250/quast_full_summary.tsv";

exp4_n="na24143 +konnector N=15"
exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_sweeps_N15/quast_full_summary.tsv";
exp4_n="na24143 +konnector --extend N=15"
exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps_N15/quast_full_summary.tsv";

###________
exp1_n="na12878 +konnector N=20"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na12878_konnector-noextend_sweeps/quast_full_summary.tsv";
#exp1_n="na12878 +konnector --extend N=20"
#exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps/quast_full_summary.tsv";

exp2_n="na12878 +konnector N=15"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na12878_konnector-noextend_sweeps_N15/quast_full_summary.tsv";
#exp2_n="na12878 +konnector --extend N=15"
#exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps_N15/quast_full_summary.tsv";

exp3_n="na12878 - abyss 2.3.1 & N[10-20] "
exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na12878_2x151/quast_full_summary.tsv";
#exp3_n="na12878 - abyss 2.3.1 & N[10-20] "
#exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24143_2x250/quast_full_summary.tsv";

#exp4_n="na12878 +konnector N=15"
#exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_sweeps_N15/quast_full_summary.tsv";
#exp4_n="na12878 +konnector --extend N=15"
#exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_sweeps_N15/quast_full_summary.tsv";
###________

exp1_n="na24143 +konnector N10 - S sweeps"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_k150-kc4_N10_S-sweeps/quast_full_summary.tsv";

exp2_n="na24143 +konnector N15 - S sweeps"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_k160-kc4-5_N=15_S-sweeps/quast_full_summary.tsv";

exp3_n="na24143 +konnector N20 - S sweeps"
exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-noextend_k150-kc4_N20_S-sweeps/quast_full_summary.tsv";

exp4_n="na24143 +konnector -- extend N15 - S sweeps"
exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers3/t3_abyss2.5/na24143_konnector-extend_k160-kc3_N15-S-sweeps/quast_full_summary.tsv";

####################################################

library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)


t2500=c(0,2500)
#
reformat <- function(kcxf, exp_n){
  #kcxf = kcxf[1:length(kcxf)-1]
  
  
  colnames(kcxf)=c("kc","k","nga50","ng50","n50", "misassemblies", "fraction","exp_n")
  
  splits=strsplit(as.character(kcxf[,2]), split="-", fixed = TRUE)
  split=splits[[1]]
  N_ind=NULL
  S_ind=NULL
  
  for (i in 1:length(split)){
    print(split[i])
    if ( grepl("N", split[i], fixed = TRUE) ){
      old_names=colnames(kcxf)
      kcxf[,length(kcxf)+1] <- NA
      colnames(kcxf)=c(old_names,"N")
      N_ind=i
    }
    if ( grepl("S", split[i], fixed = TRUE) ){
      old_names=colnames(kcxf)
      kcxf[,length(kcxf)+1] <- NA
      colnames(kcxf)=c(old_names,"S")
      S_ind=i
    }
  }
  
  df <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,2]=df[,2]
  kcxf[,1]=df[,3]
  if (! is.null(N_ind)){
    kcxf[,"N"]=df[,N_ind]
  }
  if (! is.null(S_ind)){
    S_value=df[,S_ind]
    if (grepl("/", S_value, fixed = TRUE)){
      S_value=strsplit(as.character(S_value), split="/", fixed = TRUE)
    }
    for (i in 1:length(S_value))
      kcxf[i,"S"]=S_value[[i]][1]
  }
  
  splits=strsplit(as.character(df[,2]), split = "k")
  df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,2]=as.numeric(as.character(df2[,2]))
  
  splits=strsplit(as.character(df[,3]), split = "kc")
  df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,1]=as.numeric(as.character(df2[,2]))
  
  if (! is.null(N_ind)){
    splits=strsplit(as.character(df[,N_ind]), split = "N")
    df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
    kcxf[,"N"]=as.numeric(as.character(df2[,2]))
  }
  if (! is.null(S_ind)){
    splits=strsplit(as.character(kcxf[,"S"]), split = "S")
    df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
    kcxf[,"S"]=as.numeric(as.character(df2[,2]))
  }
  
  kcxf[,"exp_n"] = exp_n
  
  return (kcxf)
}

plotter_k_vs_any <- function(experiment, data, y_par="ng50", xlimits=NULL, ylimits=NULL, breaks_count=0,
                             x_breaks=NULL,
                             x_breaks_labels=NULL,
                             y_breaks=NULL,
                             y_breaks_labels=NULL){
  
  range50 <- ggplot(data, aes_string("k",y_par , 
                                     color = "kc", 
                                     shape = "kc",#Sample,
                                     fill = "kc")) + #Algorithm)) 
    scale_color_manual(#name = "Algorithm", #"Tool", 
      #values = c("deepskyblue1","seagreen2","goldenrod1")) + #indianred2
      values = c("mediumorchid3","turquoise3","goldenrod1","orangered")) +
    scale_fill_manual(values = c("mediumorchid3","turquoise3","goldenrod1","orangered")) +
    scale_shape_manual(values = c(1, 19, 21, 19, 19))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=15)
    ) +
    xlab("k value") + ylab(as.character(y_par))
  
  #scale_y_continuous(limits = ylimits,
  #                   #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
  #                   breaks=y_breaks,
  #                   #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
  #                   #        "50 Mb","60 Mb","70 Mb","80 Mb") )
  #                   labels=y_breaks_labels )
  #labels=c("0","5","10","15","20","25","30","40", "50","60","70","80") )
  #return (range50)
  if((! is.null(ylimits)) && (! is.null(y_breaks)) && (! is.null(y_breaks_labels)) ){
    range50=range50+scale_y_continuous(limits = ylimits, breaks=y_breaks, labels = y_breaks_labels)
  } else if (! is.null(ylimits)){
    range50=range50+scale_y_continuous(limits = ylimits)
  }
  if((! is.null(xlimits)) && (! is.null(x_breaks)) ){
    range50=range50+scale_x_continuous(limits = xlimits, breaks=x_breaks)
  } else if (! is.null(xlimits)) {
    range50=range50+scale_x_continuous(limits = xlimits)
  } else if (! is.null(x_breaks)) {
    range50=range50+scale_x_continuous(breaks=x_breaks)
  }
  
  frange50 <- range50 + geom_line(aes_string(x="k", y= y_par), size=2, position=position_dodge(width=0.5)) + ggtitle(c(experiment,"k/kc sweep")) +
    theme(
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  frange50
  
  return (frange50)
}


plotter_any_vs_all <- function(experiment, data, y_par="ng50", x_par="k", color_par="kc", xlimits=NULL, ylimits=NULL, breaks_count=0,
                             x_breaks=NULL,
                             x_breaks_labels=NULL,
                             y_breaks=NULL,
                             y_breaks_labels=NULL){
  
  range50 <- ggplot(data, aes_string(x_par,y_par , 
                                     color = color_par, 
                                     shape = color_par,#Sample,
                                     fill = color_par)) + #Algorithm)) 
    scale_color_manual(#name = "Algorithm", #"Tool", 
      #values = c("deepskyblue1","seagreen2","goldenrod1")) + #indianred2
      values = c("mediumorchid3","turquoise3","goldenrod1","orangered")) +
    scale_fill_manual(values = c("mediumorchid3","turquoise3","goldenrod1","orangered")) +
    scale_shape_manual(values = c(1, 19, 21, 19, 19))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=15)
    ) +
    xlab(paste(x_par," value", sep=" ")) + ylab(as.character(y_par))
  
  #scale_y_continuous(limits = ylimits,
  #                   #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
  #                   breaks=y_breaks,
  #                   #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
  #                   #        "50 Mb","60 Mb","70 Mb","80 Mb") )
  #                   labels=y_breaks_labels )
  #labels=c("0","5","10","15","20","25","30","40", "50","60","70","80") )
  #return (range50)
  if((! is.null(ylimits)) && (! is.null(y_breaks)) && (! is.null(y_breaks_labels)) ){
    range50=range50+scale_y_continuous(limits = ylimits, breaks=y_breaks, labels = y_breaks_labels)
  } else if (! is.null(ylimits)){
    range50=range50+scale_y_continuous(limits = ylimits)
  }
  if((! is.null(xlimits)) && (! is.null(x_breaks)) ){
    range50=range50+scale_x_continuous(limits = xlimits, breaks=x_breaks)
  } else if (! is.null(xlimits)) {
    range50=range50+scale_x_continuous(limits = xlimits)
  } else if (! is.null(x_breaks)) {
    range50=range50+scale_x_continuous(breaks=x_breaks)
  }
  
  frange50 <- range50 + geom_line(aes_string(x=x_par, y= y_par), size=2, position=position_dodge(width=0.5)) + ggtitle(c(experiment,"k/kc sweep")) +
    theme(
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  frange50=frange50+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return (frange50)
}
##

exp1_f = read.table(exp1, header = FALSE, sep = "|", skip = 2)
exp2_f = read.table(exp2, header = FALSE, sep = "|", skip = 2)
exp3_f = read.table(exp3, header = FALSE, sep = "|", skip = 2)
exp4_f = read.table(exp4, header = FALSE, sep = "|", skip = 2)

kcxf=exp1_f
exp_n=exp1_n
kcxf=exp2_f
exp_n=exp2_n
df_exp1 = reformat(exp1_f, exp1_n)
df_exp2 = reformat(exp2_f, exp2_n)
df_exp3 = reformat(exp3_f, exp3_n)
df_exp4 = reformat(exp4_f, exp4_n)

dim(df_exp1)
dim(df_exp2)
dim(df_exp3)
dim(df_exp4)

# df_kc2 = reformat(kc2f, 2)
df=df_exp1
df = rbind(df_exp1,df_exp2)
df = rbind(df_exp1,df_exp2,df_exp3)
df = rbind(df_exp2,df_exp3,df_exp4)
df = rbind(df_exp1,df_exp2,df_exp3,df_exp4)

#df = rbind(df_kc2, df_kc3, df_kc4)
df[,"kc"] = as.factor(df[,"kc"])
df[,"exp_n"] = as.factor(df[,"exp_n"])
#df$tool <- factor(df$tool, levels=unique(df$tool[order(df$order)] ))
colnames(df)

############################################################
## facet_warp all together
## alternatively go to the next section for seperate plots with various k labels
n50=plotter_k_vs_any(experiment, df, "n50")
n50=plotter_any_vs_all("experiment", df, "n50", x_par="k",x_breaks = df_exp1[,"k"], x_breaks_labels = df_exp1[,"k"])
n50=plotter_any_vs_all("experiment", df, "n50", x_par="N",x_breaks = df[,"N"], x_breaks_labels = df[,"N"])
n50=plotter_any_vs_all("experiment", df, "n50", x_par="S",x_breaks = df[,"S"], x_breaks_labels = df[,"S"], xlimits = t2500)
n50 <- n50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)+theme(strip.text = element_text(size=15))

ng50=plotter_k_vs_any(experiment, df, "ng50")
ng50=plotter_any_vs_all("experiment", df, "ng50", x_par="k",x_breaks = df_exp1[,"k"], x_breaks_labels = df_exp1[,"k"])
ng50=plotter_any_vs_all("experiment", df, "ng50", x_par="N")
ng50=plotter_any_vs_all("experiment", df, "ng50", x_par="S",x_breaks = df[,"S"], x_breaks_labels = df[,"S"])
ng50 <- ng50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)+theme(strip.text = element_text(size=15))

nga50=plotter_k_vs_any(experiment, df, "nga50")
nga50=plotter_any_vs_all("experiment", df, "nga50", x_par="k",x_breaks = df_exp1[,"k"], x_breaks_labels = df_exp1[,"k"])
nga50=plotter_any_vs_all("experiment", df, "nga50", x_par="N")
nga50=plotter_any_vs_all("experiment", df, "nga50", x_par="S",x_breaks = df[,"S"], x_breaks_labels = df[,"S"], xlimits = t2500)
nga50 <- nga50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)+theme(strip.text = element_text(size=15))

miss=plotter_k_vs_any("experiment", df, "misassemblies", ylimits=c(0,2000))
miss=plotter_any_vs_all("experiment", df, "misassemblies", x_par="k",ylimits=c(0,2000), x_breaks = df_exp1[,"k"], x_breaks_labels = df_exp1[,"k"])
miss=plotter_any_vs_all("experiment", df, "misassemblies", x_par="N",ylimits=c(0,2000))
miss=plotter_any_vs_all("experiment", df, "misassemblies", x_par="S",ylimits=c(0,2000),x_breaks = df[,"S"], x_breaks_labels = df[,"S"], xlimits = t2500)
miss <- miss + facet_wrap(~exp_n, scales = "free_x", ncol = 4)+theme(strip.text = element_text(size=15))

n50
ng50
nga50
miss

library(gridExtra)

grid.arrange(nga50, miss, ncol=1)
grid.arrange(n50, ng50, nga50, miss, ncol=1)

library(ggpubr)

ggarrange(n50+ rremove("xlab"), ng50+ rremove("xlab"), nga50+ rremove("xlab"), miss,
          labels = NULL,
          align="hv",
          ncol=1, nrow=4, common.legend = TRUE, legend="right")

ggarrange(n50+ rremove("xlab")+rremove("x.text")+ggtitle("")+theme(strip.text = element_text(size=18)),
          ng50+ rremove("xlab")+rremove("x.text")+ggtitle("")+theme(strip.background = element_blank(), strip.text = element_blank()),
          nga50+ rremove("xlab")+rremove("x.text")+ggtitle("")+theme(strip.background = element_blank(), strip.text = element_blank()),
          miss+ggtitle("")+theme(strip.background = element_blank(), strip.text = element_blank()),
          labels = NULL,
          align="hv",
          ncol=1, nrow=4, common.legend = TRUE, legend="right")

############################################################
## full ggarrange
##
ylimit=c(0,125200)
ybreaks=seq(0, 125000, by = 25000)
ybreakslabs=c(c("0","25 Kb","50 Kb","75 Kb","100 Kb","125 Kb"))

ylimit=c(0,175200)
ybreaks=seq(0, 175000, by = 25000)
ybreakslabs=c(c("0","25 Kb","50 Kb","75 Kb","100 Kb","125 Kb","150 Kb","175 Kb"))

ylimit2=c(0,5200)
ybreaks2=seq(0, 3000, by = 1000)
ybreakslabs2=ybreaks2

experiment=exp1_n
xlimit=c(68,112)
xbreaks=c(seq(70, 140, by = 10))

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50",
                     xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
ng50=plotter_k_vs_any(experiment, df_temp, "ng50",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
nga50=plotter_k_vs_any(experiment, df_temp, "nga50",
                       xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit2, y_breaks = ybreaks2, y_breaks_labels = ybreakslabs2 )

exp1_pp = ggarrange(n50+ rremove("xlab")+rremove("x.text"),##+rremove("ylab")+rremove("y.text"),
                    ng50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    nga50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    miss+ ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    labels = NULL,
                    align="hv",
                    ncol=1, nrow=4, common.legend = TRUE, legend="top")
exp1_p = ggarrange(n50+ rremove("xlab")+rremove("x.text")+rremove("ylab")+rremove("y.text"),
                   ng50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   nga50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   miss+ ggtitle("")+rremove("ylab")+rremove("y.text"),
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="top")
######
experiment=exp2_n
xlimit=c(68,112)
xbreaks=c(seq(70, 140, by = 10))

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50",
                     xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
ng50=plotter_k_vs_any(experiment, df_temp, "ng50",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
nga50=plotter_k_vs_any(experiment, df_temp, "nga50",
                       xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit2, y_breaks = ybreaks2, y_breaks_labels = ybreakslabs2 )

exp2_pp = ggarrange(n50+ rremove("xlab")+rremove("x.text"),##+rremove("ylab")+rremove("y.text"),
                    ng50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    nga50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    miss+ ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    labels = NULL,
                    align="hv",
                    ncol=1, nrow=4, common.legend = TRUE, legend="top")
exp2_p = ggarrange(n50+ rremove("xlab")+rremove("x.text")+rremove("ylab")+rremove("y.text"),
                   ng50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   nga50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   miss+ ggtitle("")+rremove("ylab")+rremove("y.text"),
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="top")
######
experiment=exp3_n

xlimit=c(88,167)
xbreaks=seq(90, 250, by = 15)

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50",
                     xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
ng50=plotter_k_vs_any(experiment, df_temp, "ng50",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
nga50=plotter_k_vs_any(experiment, df_temp, "nga50",
                       xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit2, y_breaks = ybreaks2, y_breaks_labels = ybreakslabs2 )

exp3_pp = ggarrange(n50+ rremove("xlab")+rremove("x.text"),##+rremove("ylab")+rremove("y.text"),
                    ng50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    nga50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    miss+ ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    labels = NULL,
                    align="hv",
                    ncol=1, nrow=4, common.legend = TRUE, legend="top")
exp3_p = ggarrange(n50+ rremove("xlab")+rremove("x.text")+rremove("ylab")+rremove("y.text"),
                   ng50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   nga50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   miss+ ggtitle("")+rremove("ylab")+rremove("y.text"),
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="top")
######
experiment=exp4_n
xlimit=c(88,167)
xbreaks=seq(90, 250, by = 15)

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50",
                     xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
ng50=plotter_k_vs_any(experiment, df_temp, "ng50",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
nga50=plotter_k_vs_any(experiment, df_temp, "nga50",
                       xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit, y_breaks = ybreaks, y_breaks_labels = ybreakslabs )
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies",
                      xlimits = xlimit, x_breaks = xbreaks, ylimits = ylimit2, y_breaks = ybreaks2, y_breaks_labels = ybreakslabs2 )

exp4_pp = ggarrange(n50+ rremove("xlab")+rremove("x.text"),##+rremove("ylab")+rremove("y.text"),
                    ng50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    nga50+ rremove("xlab")+rremove("x.text")+ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    miss+ ggtitle(""),##+rremove("ylab")+rremove("y.text"),
                    labels = NULL,
                    align="hv",
                    ncol=1, nrow=4, common.legend = TRUE, legend="top")
exp4_p = ggarrange(n50+ rremove("xlab")+rremove("x.text")+rremove("ylab")+rremove("y.text"),
                   ng50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   nga50+ rremove("xlab")+rremove("x.text")+ggtitle("")+rremove("ylab")+rremove("y.text"),
                   miss+ ggtitle("")+rremove("ylab")+rremove("y.text"),
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="top")
##################
##################

full_plot = ggarrange(exp1_p,exp2_p,exp3_p,exp4_p,
                      common.legend = TRUE, legend="top", ncol = 4)

file_name=paste(species, "_abyss_k-kc-sweeps.pdf", sep = "", collapse = NULL)
ggsave(plot = full_plot, width = 14, height = 10, dpi = 300, filename = file_name)
ggsave(plot = exp1_pp, width = 14, height = 10, dpi = 300, filename = "tags---.pdf")

######
##################################################################
## memory
stop = 1
n50_100 = n50
ng50_100 = ng50
nga50_100 = nga50
miss_100 = miss

n50_150 = n50
ng50_150 = ng50
nga50_150 = nga50
miss_150 = miss

n50_301 = n50
ng50_301 = ng50
nga50_301 = nga50
miss_301 = miss

ggarrange(n50_100+ rremove("xlab")+rremove("x.text"),n50_150+ rremove("xlab")+rremove("ylab")+rremove("xy.text"),n50_301+ rremove("xlab")+rremove("ylab")+rremove("xy.text"),
          ng50_100+ rremove("xlab")+rremove("x.text"), ng50_150+ rremove("xlab")+rremove("ylab")+rremove("xy.text"), ng50_301+ rremove("xlab")+rremove("ylab")+rremove("xy.text"),
          nga50_100+ rremove("xlab")+rremove("x.text"), nga50_150+ rremove("xlab")+rremove("ylab")+rremove("xy.text"), nga50_301+ rremove("xlab")+rremove("ylab")+rremove("xy.text"), 
          miss_100,miss_150+rremove("ylab")+rremove("y.text"),miss_301+rremove("ylab")+rremove("y.text"),
          labels = NULL,
          align="hv",
          ncol=3, nrow=4, common.legend = TRUE, legend="right")


####################


library(reshape2)
df2=melt(df,"k")
ggplot(df2, aes(k,value))+ geom_point()+facet_grid(.~variable, scale="free_x")
