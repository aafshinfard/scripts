library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )

### Inputs:
#experiment="abyss2.5 2x100"
ad_reads2x100="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na12878.tsv";
ad_reads2x150="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24631.tsv";
ad_reads2x301="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24143.tsv";
ad_na24835="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24835.tsv";

ad_na12878="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na12878.tsv";
ad_na24631="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24631.tsv";
ad_na24143="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24143.tsv";
ad_na24835="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24835.tsv";

na12878 = read.table(ad_na12878, header = FALSE, sep = "|", skip = 0, as.is = c(2,3,4,5,6,7,8,9,10),
                     colClasses = c("character","character","numeric","numeric","numeric","numeric","character","character","character","character","character"))
na24631 = read.table(ad_na24631, header = FALSE, sep = "|", skip = 0, as.is = c(2,3,4,5,6,7,8,9,10),
                     colClasses = c("character","character","numeric","numeric","numeric","numeric","character","character","character","character","character"))
na24143 = read.table(ad_na24143, header = FALSE, sep = "|", skip = 0, as.is = c(2,3,4,5,6,7,8,9,10),
                     colClasses = c("character","character","numeric","numeric","numeric","numeric","character","character","character","character","character"))
na24385 = read.table(ad_na24385, header = FALSE, sep = "|", skip = 0, as.is = c(2,3,4,5,6,7,8,9,10),
                     colClasses = c("character","character","numeric","numeric","numeric","numeric","character","character","character","character","character"))


reads2x100 = read.table(ad_reads2x100, header = FALSE, sep = "|", skip = 1, as.is = c(2,3,4,5,6,7,8,9,10),
                        colClasses = c("character","character","num.with.commas","num.with.commas","num.with.commas","numeric","character","character","character","character","character"))

results=na12878
results=reads2x100
reformat <- function(results, experiment){
  results = results[2:(length(results)-4)]
  colnames(results)=c("tool","nga50","ng50","n50", "misassemblies", "fraction")
  class(results$misassemblies[1])
  
  results[,"tool.order"]=as.factor(c(1,2,3,6,5))
  results$tool <- factor(results$tool, levels = unique(results$tool[order(results$tool.order)]))
  return (results)
}


d_na12878=reformat(na12878)
d_na24631=reformat(na24631)
d_na24143=reformat(na24143)
d_na24385=reformat(na24385)

reads100=reformat(reads2x100)





plotter1 <- function(experiment, data, xlimits, ylimits, breaks_count=0,
                     x_breaks=c(0,500,1000,1500,2000,2500,3000, 3500, 4000, 4500, 5000 ),
                     x_breaks_labels=c("0","500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" ),
                     y_breaks=c(0,25e3,50e3,75e3,100e3,125e3,150e3,175e3,200e3),
                     y_breaks_labels=c("0","25k","50k","75k","100k","125k","150k","175k","200k")){
  if(breaks_count > 0){
    digcount=1
    a=breaks_count
    while (a>10){
      a=a/10
      digcount=digcount+1
    }
  }
  
  range50 <- ggplot(data, aes(`misassemblies`, ng50, width = width, 
                              color = tool, 
                              shape = tool))+#,#Sample,
    #fill = Sample)) + #Algorithm)) 
    scale_color_manual(name = "tool", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       values = c("gray","darkgoldenrod1","darkorange3","mediumorchid3","turquoise3","black")) +
    #scale_fill_manual(values = c("gray","dodgerblue2","darkorange1","mediumorchid3","turquoise3","black"), guide = FALSE) +
    scale_shape_manual(name = "tool", #"Dataset",
                       values = c(5,8, 17, 15, 19, 20))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Number of Quast Misassemblies") + ylab("NGA50|--|NG50")+
    scale_x_continuous(limits = xlimits,
                       breaks=x_breaks, labels=x_breaks_labels)+
    scale_y_continuous(limits = ylimits,
                       #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
                       breaks=y_breaks,
                       #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
                       #        "50 Mb","60 Mb","70 Mb","80 Mb") )
                       labels=y_breaks_labels )
  #labels=c("0","5","10","15","20","25","30","40", "50","60","70","80") )
  
  frange50 <- range50 + geom_errorbar(aes(ymin=ng50, ymax= nga50),size=2,  
                                      #width =(xlimits[2]-xlimits[1])/13,
                                      position=position_dodge(width=0.2)) + ggtitle(experiment) +
    theme(
      strip.text = element_text(size=12),
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  return (frange50)
}
# dodge
library(dplyr)

dodger <- function(results){
  results2 <- results %>%
    group_by(misassemblies) %>%
    mutate(
      width = 400 * n()
    )
  return (results2)
}


# dodge



reads100=reformat(reads2x100)
d_na12878=reformat(d_na12878)

d_na12878=d_na12878[order(d_na12878$tool.order),]
d_na12878=d_na12878[order(d_na12878$tool.order),]
d_na12878=d_na12878[order(d_na12878$tool.order),]
d_na12878=d_na12878[order(d_na12878$tool.order),]

reads100=reads100[order(reads100$tool.order),]

na12878=dodger(d_na12878)
reads100=dodger(reads100)


ylimits_general = c(0,205000)


#aq300sub = aq[aq[,'Seq..and.Assembly']=="2x300sub",]

xlimits = c(-1,5000)
xbreaks = seq(0, 5000, by = 1000)
xbreaks_l = xbreaks
#xbreaks_l = c("500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" )

ylimits = c(0,205000)
#ylimits = ylimits_general
experiment = "na12878"
na12878_plot = plotter1(experiment, na12878, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
reads100_plot = plotter1(experiment, reads100, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
reads100_plot

experiment = "2x150"
reads150_plot = plotter1(experiment, reads150, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
reads150_plot

experiment = "2x301"
reads301_plot = plotter1(experiment, reads301, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
reads301_plot

library(gridExtra)

grid.arrange(reads100_plot  , reads150_plot  , reads301_plot  , ncol=3 )

library(ggpubr)

ggarrange(reads100_plot+ rremove("xlab"), reads150_plot+ rremove("ylab"), reads301_plot+ rremove("xlab")+ rremove("ylab"),
          labels = NULL,
          align="hv",
          ncol=3, nrow=1, common.legend = TRUE, legend="right")

## Old but gold
## Old but gold
## Old but gold
## Old but gold
## Old but gold
## Old but gold
## Old but gold
## Old but gold

plotter1 <- function(experiment, data, xlimits, ylimits, breaks_count=0,
                     x_breaks=c(0,500,1000,1500,2000,2500,3000, 3500, 4000, 4500, 5000 ),
                     x_breaks_labels=c("0","500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" ),
                     y_breaks=c(0,25e3,50e3,75e3,100e3,125e3,150e3,175e3,200e3),
                     y_breaks_labels=c("0","25","50","75","100","125","150","175","200")){
  if(breaks_count > 0){
    digcount=1
    a=breaks_count
    while (a>10){
      a=a/10
      digcount=digcount+1
    }
  }
  
  range50 <- ggplot(data, aes(`Misassemblies.Count`, NG50, 
                              color = Algorithm, 
                              shape = Algorithm))+#,#Sample,
    #fill = Sample)) + #Algorithm)) 
    scale_color_manual(name = "Assembly", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       values = c("dodgerblue2","darkorange1","mediumorchid3","turquoise3","black")) +
    #scale_fill_manual(values = c("dodgerblue2","darkorange1","mediumorchid3","turquoise3","black"), guide = FALSE) +
    scale_shape_manual(name = "Assembly", #"Dataset",
                       values = c(8, 17, 15, 19, 20))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Number of Quast Misassemblies") + ylab("NGA50|--|NG50")+
    scale_x_continuous(limits = xlimits,
                       breaks=x_breaks, labels=x_breaks_labels)+
    scale_y_continuous(limits = ylimits,
                       #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
                       breaks=y_breaks,
                       #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
                       #        "50 Mb","60 Mb","70 Mb","80 Mb") )
                       labels=y_breaks_labels )
  #labels=c("0","5","10","15","20","25","30","40", "50","60","70","80") )
  
  frange50 <- range50 + geom_errorbar(aes(ymin=NG50, ymax= NGA50),size=2,  width =(xlimits[2]-xlimits[1])/13) + ggtitle(experiment) +
    theme(
      strip.text = element_text(size=12),
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  return (frange50)
}


plotter2 <- function(experiment, data, xlimits, ylimits, breaks_count=0,
                     x_breaks=c(0,500,1000,1500,2000,2500,3000, 3500, 4000, 4500, 5000 ),
                     x_breaks_labels=c("0","500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" ),
                     y_breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
                     y_breaks_labels=c("0","","10","","20","","30","40","50","60","70","80")){
  if(breaks_count > 0){
    digcount=1
    a=breaks_count
    while (a>10){
      a=a/10
      digcount=digcount+1
    }
  }
  range50 <- ggplot(data, aes(`Misassemblies.Count`, NG50, 
                              color = Algorithm, 
                              shape = Algorithm,#Sample,
                              fill = Sample)) + #Algorithm)) 
    scale_color_manual(name = "Assembly", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       values = c("dodgerblue2","darkorange1","mediumorchid3","turquoise3","black")) +
    scale_fill_manual(values = c("dodgerblue2","darkorange1","mediumorchid3","turquoise3","black"), guide = FALSE) +
    scale_shape_manual(name = "Assembly", #"Dataset",
                       values = c(8, 17, 15, 19, 20))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Number of Quast Misassemblies") + ylab("NGA50|--|NG50")
  
  
  range50 <- ggplot(data, aes(`Misassemblies.Count`, NG50, 
                              color = Algorithm, 
                              shape = Algorithm,#Sample,
                              fill = Sample)) + #Algorithm)) 
    scale_color_manual(name = "Assembly", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       values = c("black","mediumorchid3","turquoise3","goldenrod1")) +
    scale_fill_manual(values = c("black","mediumorchid3","turquoise3","goldenrod1"), guide = FALSE) +
    scale_shape_manual(name = "Assembly", #"Dataset",
                       values = c(8, 17, 15, 19))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Number of Quast Misassemblies")
  
  frange50 <- range50 + geom_errorbar(aes(ymin=NG50, ymax= NGA50),size=2)+ ggtitle(experiment) +
    theme(
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  return (frange50)
}

##








############################
NGA50_all=c(
  
  # 2x300sub
  121938,
  112233,
  113844,
  0,
  113432,
  
  # 2x250
  107620,
  86949,
  108838,
  0,
  88513,
  
  # 2x100
  23528,
  25418,
  8431,
  0,
  10245
)

NG50_all=c(
  # 2x300sub
  178036,
  148359,
  176774,
  0,
  204193,
  # 2x250
  124961,
  120800,
  157592,
  0,
  133563,
  
  # 2x100
  23576,
  25418,
  8504,
  0,
  10379
)

N50_all=c(
  # 2x300sub
  178036,
  173974,
  178442,
  0,
  204193	,
  
  # 2x250
  124961,
  120800,
  157592,
  0,
  133563,
  
  # 2x100
  25510,
  27583,
  9321,
  0,
  11343
)

misassemblies_all=c(
  # 2x300sub
  17, 16, 21, 0, 24,
  # 2x250
  15, 15, 26, 0, 29,
  # 2x100
  5, 7, 4, 0, 4
)
basetech_all=
  as.factor(c(
    "2x300sub","2x300sub","2x300sub","2x300sub","2x300sub",
    "2x250","2x250","2x250","2x250","2x250",
    "2x100","2x100","2x100","2x100","2x100"
  ))
algorithms_all=c(
  "SPAdes", "Unicycler", "ABySS 2.5", "ABySS 2.x", "ABySS 2.0",
  "SPAdes", "Unicycler", "ABySS 2.5", "ABySS 2.x", "ABySS 2.0",
  "SPAdes", "Unicycler", "ABySS 2.5", "ABySS 2.x", "ABySS 2.0"
)
sample_all=
  as.factor(c(
    "2x300sub","2x300sub","2x300sub","2x300sub","2x300sub",
    "2x250","2x250","2x250","2x250","2x250",
    "2x100","2x100","2x100","2x100","2x100"
  )
  )

algorder_all=c(
  "1","2","3","4","5",
  "1","2","3","4","5",
  "1","2","3","4","5"
)

samporder_all=c(
  #Huh?
  "1","1","1","1","1",
  "2","2","2","2","2",
  "3","3","3","3","3"
)

aq = data.frame(
  "alg.order"=algorder_all,
  "samp.order"=samporder_all,
  "Algorithm"=algorithms_all,
  "Sample"=sample_all,
  "Seq. and Assembly"=basetech_all,
  "NG50"=NG50_all, "NGA50"=NGA50_all,
  "Misassemblies Count"=misassemblies_all
)

aq$Algorithm <- factor(aq$Algorithm, levels = unique(aq$Algorithm[order(aq$alg.order)]))
aq$Sample <- factor(aq$Sample, levels = unique(aq$Sample[order(aq$samp.order)]))
aq$Seq..and.Assembly <-factor(aq$Seq..and.Assembly, levels = unique(aq$Seq..and.Assembly[order(aq$samp.order)]))

aq


##############################################################
##############################################################

ylimits = c(0,205000)


xbreaks = c(0,5,10,15,20, 25, 30)
xbreaks_l = xbreaks

###############################
experiment = "2x300sub"
aq300sub = aq[aq[,'Seq..and.Assembly']=="2x300sub",]

xlimits = c(0,35)
xbreaks = c(0,5,10,15,20, 25, 30)
xbreaks_l = xbreaks
#xbreaks_l = c("500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" )

aq300sub_plot = plotter1(experiment, aq300sub, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
aq300sub_plot
ggsave(plot = aq300sub_plot, width = 5.5, height = 6, dpi = 300, filename = "aq300sub_plot.pdf")
###############################
experiment = "2x250"
aq250 = aq[aq[,'Seq..and.Assembly']=="2x250",]

xlimits = c(0,35)
xbreaks = c(0,5,10,15,20, 25, 30)
xbreaks_l = xbreaks
#xbreaks_l = c("500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" )

aq250_plot = plotter1(experiment, aq250, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
aq250_plot
ggsave(plot = aq250_plot, width = 5.5, height = 6, dpi = 300, filename = "aq250_plot.pdf")
###############################
experiment = "2x100"
aq100 = aq[aq[,'Seq..and.Assembly']=="2x100",]

xlimits = c(0,35)
xbreaks = c(0,5,10,15,20, 25, 30)
xbreaks_l = xbreaks
#xbreaks_l = c("500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" )
#ylimits = c(0,50000)
aq100_plot = plotter1(experiment, aq100, xlimits, ylimits, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
aq100_plot
ggsave(plot = aq100_plot, width = 5.5, height = 6, dpi = 300, filename = "aq100_plot.pdf")


##############################################################
############################################################## DONE
##############################################################

experiment="E.coli"
data = aq


data = aq[,colnames(aq) != "Seq..and.Assembly"]
data = aq[,colnames(aq) != "Sample"]
xlimits = c(0,35)
xbreaks = c(0,5,10,15,20, 25, 30)
xbreaks_l = xbreaks
plot_all = plotter1(experiment, data, xlimits, ylimits, x_breaks=xbreaks, x_breaks_labels = xbreaks) + facet_wrap( ~ Seq..and.Assembly, scale="free_x")

plot_all







plot_all = plotter2(experiment, data, xlimits, ylimits) + facet_wrap(Sample ~ Seq..and.Assembly, scale="free_x")
plot_all

range50 <- ggplot(data, aes(`Misassemblies.Count`, NG50, 
                            color = Algorithm, 
                            shape = Algorithm,#Sample,
                            fill = Sample)) + #Algorithm)) 
  scale_color_manual(name = "Assembly", #"Tool", 
                     #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                     values = c("black","turquoise3","mediumorchid3","goldenrod1")) +
  scale_fill_manual(values = c("black","turquoise3","mediumorchid3","goldenrod1"), guide = FALSE) +
  scale_shape_manual(name = "Assembly", #"Dataset",
                     values = c(8, 17, 15, 19))+
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 22, colour = "black"),
    legend.title = element_text(size=15),
    legend.text = element_text(size=14)
  ) +
  xlab("Number of Quast Misassemblies") +
  scale_x_continuous(limits = xlimits,
                     #breaks=c(0,200,400,600,800,1000,1500,2000,2500,2900,3000, 3500 ))+
                     breaks=c(0,250,500,750,1000,1500,2000,2500,3000, 3500, 4000, 4500, 5000 ),
                     #labels=c("0","250","500","750", "1k", "1.5k", "2k", "2.5k", "3k", "3.5k", "4k" ))+
                     labels=c("0",".25",".50",".75", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5" ))+
  scale_y_continuous(limits = c(0,80e6),
                     breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
                     #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
                     #        "50 Mb","60 Mb","70 Mb","80 Mb") )
                     labels=c("0","5","10","15","20","25","30","40",
                              "50","60","70","80") )

frange50 <- range50 + geom_errorbar(aes(ymin=NG50, ymax= NGA50),size=2,  width =(xlimits[2]-xlimits[1])/12) + ggtitle(experiment) +
  theme(
    panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                      lineend="round"),  # "round", "butt" or "square"
    panel.grid.major.x = element_line( size=0.5, linetype=1,
                                       lineend="round"),
    panel.grid.minor.y = element_line( size=0.5 ),
    panel.grid.minor.x = element_blank(),
  )

frange50
