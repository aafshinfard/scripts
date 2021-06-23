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

exp1_n="na12878 2x151"
exp1="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na12878_2x151/quast_full_summary.tsv";

exp2_n="na24631 2x151"
exp2="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24631_2x151/quast_full_summary.tsv";

exp3_n="na24143 2x250"
exp3="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24143_2x250/quast_full_summary.tsv";

exp4_n="na24385 2x250"
exp4="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/numbers2/abyss2.5/na24385_2x250/quast_full_summary.tsv";

####################################################

library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

#
reformat <- function(kcxf, exp_n){
  #kcxf = kcxf[1:length(kcxf)-1]
  colnames(kcxf)=c("kc","k","nga50","ng50","n50", "misassemblies", "fraction","exp_n")
  
  splits=strsplit(as.character(kcxf[,2]), split = "-", fixed = TRUE)
  
  df <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,2]=df[,2]
  kcxf[,1]=df[,3]
  
  splits=strsplit(as.character(df[,2]), split = "k")
  df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,2]=as.numeric(as.character(df2[,2]))
  
  splits=strsplit(as.character(df[,3]), split = "kc")
  df2 <- data.frame(matrix(unlist(splits), nrow=length(splits), byrow=TRUE))
  kcxf[,1]=as.numeric(as.character(df2[,2]))
  kcxf[,length(kcxf)] = exp_n
  return (kcxf)
}

plotter_k_vs_any <- function(experiment, data, y_par="ng50", xlimits=NULL, ylimits=0, breaks_count=0,
                             x_breaks=NULL,
                             x_breaks_labels=NULL,
                             y_breaks=c(0,50,100,150,200,250, 300, 350,400 ),
                             y_breaks_labels=c(0,50,100,150,200,250, 300, 350,400 )){
  
  range50 <- ggplot(data, aes_string("k",y_par , 
                                   color = "kc", 
                                   shape = "kc",#Sample,
                                   fill = "kc")) + #Algorithm)) 
    scale_color_manual(#name = "Algorithm", #"Tool", 
      #values = c("deepskyblue1","seagreen2","goldenrod1")) + #indianred2
      values = c("mediumorchid3","turquoise3","goldenrod1")) +
    scale_fill_manual(values = c("mediumorchid3","turquoise3","goldenrod1")) +
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

##

exp1_f = read.table(exp1, header = FALSE, sep = "|", skip = 2)
exp2_f = read.table(exp2, header = FALSE, sep = "|", skip = 2)
exp3_f = read.table(exp3, header = FALSE, sep = "|", skip = 2)
exp4_f = read.table(exp4, header = FALSE, sep = "|", skip = 2)

df_exp1 = reformat(exp1_f, exp1_n)
df_exp2 = reformat(exp2_f, exp2_n)
df_exp3 = reformat(exp3_f, exp3_n)
df_exp4 = reformat(exp4_f, exp4_n)

dim(df_exp1)
dim(df_exp2)
dim(df_exp3)
dim(df_exp4)

# df_kc2 = reformat(kc2f, 2)

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
n50 <- n50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)

ng50=plotter_k_vs_any(experiment, df, "ng50")
ng50 <- ng50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)

nga50=plotter_k_vs_any(experiment, df, "nga50")
nga50 <- nga50 + facet_wrap(~exp_n, scales = "free_x", ncol = 4)

miss=plotter_k_vs_any(experiment, df, "misassemblies")
miss <- miss + facet_wrap(~exp_n, scales = "free_x", ncol = 4)

n50
ng50
nga50
miss

library(gridExtra)

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
experiment=exp1_n
xlimit=c(68,112)
xbreaks=c(seq(70, 140, by = 10))

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50", xlimits = xlimit, x_breaks = xbreaks)
ng50=plotter_k_vs_any(experiment, df_temp, "ng50", xlimits = xlimit, x_breaks = xbreaks)
nga50=plotter_k_vs_any(experiment, df_temp, "nga50", xlimits = xlimit, x_breaks = xbreaks)
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies", xlimits = xlimit, x_breaks = xbreaks)

exp1_p = ggarrange(n50+ rremove("xlab"), ng50+ rremove("xlab"), nga50+ rremove("xlab"), miss,
          labels = NULL,
          align="hv",
          ncol=1, nrow=4, common.legend = TRUE, legend="right")
######
experiment=exp2_n
xlimit=c(68,112)
xbreaks=c(seq(70, 140, by = 10))

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50", xlimits = xlimit, x_breaks = xbreaks)
ng50=plotter_k_vs_any(experiment, df_temp, "ng50", xlimits = xlimit, x_breaks = xbreaks)
nga50=plotter_k_vs_any(experiment, df_temp, "nga50", xlimits = xlimit, x_breaks = xbreaks)
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies", xlimits = xlimit, x_breaks = xbreaks)

exp2_p = ggarrange(n50+ rremove("xlab"), ng50+ rremove("xlab"), nga50+ rremove("xlab"), miss,
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="right")
######
experiment=exp3_n
xlimit=c(88,167)
xbreaks=seq(90, 250, by = 15)

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50", xlimits = xlimit, x_breaks = xbreaks)
ng50=plotter_k_vs_any(experiment, df_temp, "ng50", xlimits = xlimit, x_breaks = xbreaks)
nga50=plotter_k_vs_any(experiment, df_temp, "nga50", xlimits = xlimit, x_breaks = xbreaks)
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies", xlimits = xlimit, x_breaks = xbreaks)

exp3_p = ggarrange(n50+ rremove("xlab"), ng50+ rremove("xlab"), nga50+ rremove("xlab"), miss,
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="right")
######
experiment=exp4_n
xlimit=c(88,167)
xbreaks=seq(90, 250, by = 30)

df_temp = df[df[,"exp_n"]==experiment,]

n50=plotter_k_vs_any(experiment, df_temp, "n50", xlimits = xlimit, x_breaks = xbreaks)
ng50=plotter_k_vs_any(experiment, df_temp, "ng50", xlimits = xlimit, x_breaks = xbreaks)
nga50=plotter_k_vs_any(experiment, df_temp, "nga50", xlimits = xlimit, x_breaks = xbreaks)
miss=plotter_k_vs_any(experiment, df_temp, "misassemblies", xlimits = xlimit, x_breaks = xbreaks)

exp4_p = ggarrange(n50+ rremove("xlab"), ng50+ rremove("xlab"), nga50+ rremove("xlab"), miss,
                   labels = NULL,
                   align="hv",
                   ncol=1, nrow=4, common.legend = TRUE, legend="right")
##################
##################

ggarrange(exp1_p,exp2_p,exp3_p,exp4_p,
          common.legend = TRUE, legend="right", ncol = 4)
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
