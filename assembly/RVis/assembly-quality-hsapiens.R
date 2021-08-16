library(ggpubr)
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
ad_na24385="/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens/experiments/results_na24385.tsv";

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
  
  results[,"tool.order"]=as.factor(c(4,5,1,2,3))
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
                       values = c("royalblue2","darkgoldenrod1","darkorange3","mediumorchid3","turquoise3","black")) +
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
d_na24631=d_na24631[order(d_na24631$tool.order),]
d_na24143=d_na24143[order(d_na24143$tool.order),]
d_na24385=d_na24385[order(d_na24385$tool.order),]



reads100=reads100[order(reads100$tool.order),]

na12878=dodger(d_na12878)
na24631=dodger(d_na24631)
na24143=dodger(d_na24143)
na24385=dodger(d_na24385)




reads100=dodger(reads100)



##########################################################################
##########################################################################
##########################################################################
##################### Manual changes:


levels(na12878$tool)=c(levels(na12878$tool),"Abyss-new")
na12878[2,'tool']="Abyss-new"
na12878[2,'ng50']=81524
na12878[2,'nga50']=80442
na12878[2,'misassemblies']=713

levels(na24631$tool)=c(levels(na24631$tool),"Abyss-new")
na24631[2,'tool']=as.factor("Abyss-new")
na24631[2,'ng50']=68667
na24631[2,'nga50']=68120
na24631[2,'misassemblies']=513

levels(na24143$tool)=c(levels(na24143$tool),"Abyss-new")
na24143[2,'tool']="Abyss-new"
na24143[2,'ng50']=72967
na24143[2,'nga50']=72550
na24143[2,'misassemblies']=326

levels(na24385$tool)=c(levels(na24385$tool),"Abyss-new")
na24385[2,'tool']="Abyss-new"
na24385[2,'ng50']=77884
na24385[2,'nga50']=77309
na24385[2,'misassemblies']=426


ylimits_general = c(0,200000)


#aq300sub = aq[aq[,'Seq..and.Assembly']=="2x300sub",]

xlimits = c(0,2200)
xbreaks = seq(0, 5000, by = 500)
xbreaks_l = xbreaks
#xbreaks_l = c("500", "1000", "1500", "2000", "2500", "3000", "3500", "4000", "4500", "5000" )

ylimits1 = c(0,100000)
ylimits2 = c(0,195000)
#ylimits = ylimits_general

experiment = "na12878"
legend_na12878_plot = plotter1(experiment, na12878, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
ggsave(plot = legend_na12878_plot, width = 5.5, height = 6, dpi = 300, filename = "hsapiens_legend1.pdf")

experiment = "na24631"
legend_na24631_plot = plotter1(experiment, na24631, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
ggsave(plot = legend_na24631_plot, width = 5.5, height = 6, dpi = 300, filename = "hsapiens_legend1.pdf")

xlimits = c(0,1600)
experiment = "na24385"
legend_na24385_plot = plotter1(experiment, na24385, xlimits, ylimits2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
ggsave(plot = legend_na24385_plot, width = 5.5, height = 6, dpi = 300, filename = "hsapiens_legend1.pdf")
xlimits = c(0,1200)
experiment = "na24143"
legend_na24143_plot = plotter1(experiment, na24143, xlimits, ylimits2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
ggsave(plot = legend_na24143_plot, width = 5.5, height = 6, dpi = 300, filename = "hsapiens_legend2.pdf")

experiment = "na12878"
na12878_plot = plotter1(experiment, na12878, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)+
  theme(legend.position = "none")+
  rremove("x.text")+rremove("xlab")+
  rremove("y.text")+rremove("ylab")

experiment = "na24631"
na24631_plot = plotter1(experiment, na24631, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)+
  theme(legend.position = "none")+
  rremove("x.text")+rremove("xlab")+
  rremove("y.text")+rremove("ylab")

experiment = "na24143"
na24143_plot = plotter1(experiment, na24143, xlimits, ylimits2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)+
  theme(legend.position = "none")+
  rremove("x.text")+rremove("xlab")+
  rremove("y.text")+rremove("ylab")

experiment = "na24385"
na24385_plot = plotter1(experiment, na24385, xlimits, ylimits2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)+
  theme(legend.position = "none")+
  rremove("x.text")+rremove("xlab")+
  rremove("y.text")+rremove("ylab")

library(gridExtra)

full_hsapiens = grid.arrange(na12878_plot  , na24631_plot  , na24143_plot, na24385_plot  , ncol=2 )
ggsave(plot = full_hsapiens, width = 11, height = 12, dpi = 300, filename = "hsapiens_quality.pdf")



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



