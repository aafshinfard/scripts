##### Assembly quality visualization:
##### Results file should be in a specific format: more info will be added later but I copy-paste and then use "polish-commands" script in the to make it
##### ./scripts/temp/polish-commands.sh
##### Amirhossein Afshinfard aafshinfard@gmail.com

##### serach for: "# To change:" to find the section you need changes in the script

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


ad_all="/projects/btl/aafshinfard/github/scripts/temp/results_final.tsv";

table = read.table(ad_all, strip.white=TRUE, header = FALSE, sep="|", skip=0,
                   colClasses = rep("character",11))



table$V5 = as.numeric(str_trim(table$V5))
table$V6 = as.numeric(str_trim(table$V6))
table$V7 = as.numeric(str_trim(table$V7))
table$V8 = as.numeric(str_trim(table$V8))
table$V9 = as.numeric(str_trim(table$V9))
table$V10 = str_trim(table$V10)
table$V11 = as.numeric(str_trim(table$V11))
table$V12 = str_trim(table$V12)

table=table[table$V12!="white",]

results=table
reformat <- function(results, experiment){
  results = results[,2:12]
  colnames(results)=c("dataset","tool.order","tool","nga50","ng50","n50", "misassemblies", "fraction","time","memory","color")
  
  results[,"tool.order"]=as.factor(results[,"tool.order"])
  results$tool <- factor(results$tool, levels = unique(results$tool[order(results$tool.order)]))
  #results$color <- factor(results$color, levels = unique(results$color[order(results$tool.order)]))
  return (results)
}

d_all=reformat(table)

plotter1 <- function(experiment, data, xlimits, ylimits, breaks_count=0,
                     x_breaks=seq(0, 5000, by = 100),
                     x_breaks_labels=seq(0, 5000, by = 100),
                     y_breaks=seq(0, 200000, by = 10000),
                     y_breaks_labels=c("0","10k","20k","30k","40k","50k","60k","70k","80k","90k","100k","110k","120k","130k","140k","150k","160k","170k","180k","190k","200k"),
                     rotate = TRUE){
  if(breaks_count > 0){
    digcount=1
    a=breaks_count
    while (a>10){
      a=a/10
      digcount=digcount+1
    }
  }
  
  range50 <- ggplot(data, aes(`misassemblies`, ng50, 
                              color = tool, 
                              #shape = tool
                              #fill = color,
                              width = width))+#,#Sample,
    #fill = Sample)) + #Algorithm)) 
    scale_color_manual(name = "tool", #"Tool"
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       #values = c("royalblue2","darkgoldenrod1","darkorange3","mediumorchid3","turquoise3","black","grey")) +
                       #values = data$color[order(data$tool.order)]) +
                       breaks = data$tool,
                       values = data$color) +
    #data$color[order(data$tool.order)]
    #scale_fill_manual(values = c("gray","dodgerblue2","darkorange1","mediumorchid3","turquoise3","black"), guide = FALSE) +
    #scale_shape_manual(name = "tool", #"Dataset",
    #                   values = c(5,8, 17, 15, 19, 20, 21))+
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
    ) + 
    geom_text(aes(label = paste0(round(data$fraction, digits=1), "%")), nudge_y = (ylimits[2]-ylimits[1])/50 )
  if (rotate == TRUE)
    frange50 = frange50 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return (frange50)
}
# dodge
library(dplyr)

dodger <- function(results){
  results2 <- results %>%
    group_by(misassemblies) %>%
    mutate(
      width = (dodger_width * n())
    )
  return (results2)
}

dodger2 <- function(results){
  results2 <- results %>%
    group_by(misassemblies) %>%
    mutate(
      #width = 1.2 * n()
      width = dodger2_width * n()
    )
  return (results2)
}
d_all

############################################################################
#### To change:

d_1=d_all[d_all$dataset=="na12878",]
d_2=d_all[d_all$dataset=="na24631",]
d_3=d_all[d_all$dataset=="OSativa",]
d_4=d_all[d_all$dataset=="Ecoli 2x100",]
d_5=d_all[d_all$dataset=="Ecoli 2x150",]
d_6=d_all[d_all$dataset=="Ecoli 2x301",]


dodger_width=400
dodger2_width=2.5


ylimits_general = c(25000,200000)
xlimits = c(0,2200)
xbreaks = seq(0, 5000, by = 500)
xbreaks_l = xbreaks


ylimits1 = c(25000,100000)

xlimits2 = c(0,2500)
ylimits2 = c(0,45000)
y_breaks2=seq(0, 45000, by = 10000)
y_breaks_l2=c("0","10k","20k","30k","40k")

xlimits3 = c(-3,20)
xbreaks3 = seq(0, 20, by = 5)

ylimits3 = c(80000,210000)
y_breaks3 = seq(0, 250000, by = 25000)

############################################################################
#### May change if you change number of datasets:
d_1=d_1[order(d_1$tool.order),]
d_2=d_2[order(d_2$tool.order),]
d_3=d_3[order(d_3$tool.order),]
d_4=d_4[order(d_4$tool.order),]
d_5=d_5[order(d_5$tool.order),]
d_6=d_6[order(d_6$tool.order),]
d_7=d_7[order(d_7$tool.order),]
d_8=d_8[order(d_8$tool.order),]

df_1=dodger(d_1)
df_2=dodger(d_2)
df_3=dodger(d_3)
df_4=dodger2(d_4)
df_5=dodger2(d_5)
df_6=dodger2(d_6)
df_7=dodger(d_7)
df_8=dodger(d_8)

############################################################################
##################### More possible manual changes:

#levels(na12878$tool)=c(levels(na12878$tool),"Abyss-new")
#na12878[2,'tool']="Abyss-new"
#na12878[2,'ng50']=81524
#na12878[2,'nga50']=80442
#na12878[2,'misassemblies']=713

############################################################################
############################################################################
### Vis pack 1:

#ylimits = ylimits_general

#df_6$color[8]="indianred2"

#df_4$color[5]="lightblue"
#df_5$color[5]="lightblue"
#df_6$color[5]="lightblue"


#df_4$color[6]="royalblue1"
#df_5$color[6]="royalblue1"
#df_6$color[6]="royalblue1"


results$color <- factor(results$color, levels = unique(results$color[order(results$tool.order)]))

p_1 = plotter1(df_1$dataset[1], df_1, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
p_2 = plotter1(df_2$dataset[1], df_2, xlimits, ylimits1, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
p_3 = plotter1(df_3$dataset[1], df_3, xlimits2, ylimits2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l, y_breaks = y_breaks2, y_breaks_labels = y_breaks_l2)
p_4 = plotter1(df_4$dataset[1], df_4, xlimits3, ylimits3, x_breaks = xbreaks3, x_breaks_labels = xbreaks3, rotate = FALSE)#, y_breaks = y_breaks3, y_breaks_labels = y_breaks3)
p_5 = plotter1(df_5$dataset[1], df_5, xlimits3, ylimits3, x_breaks = xbreaks3, x_breaks_labels = xbreaks3, rotate = FALSE)#, y_breaks = y_breaks3, y_breaks_labels = y_breaks3)
p_6 = plotter1(df_6$dataset[1], df_6, xlimits3, ylimits3, x_breaks = xbreaks3, x_breaks_labels = xbreaks3, rotate = FALSE)#, y_breaks = y_breaks3)#, y_breaks_labels = y_breaks3)


library(gridExtra)

legend_h <- cowplot::get_legend(p_1)
legend_o <- cowplot::get_legend(p_3)
legend_e <- cowplot::get_legend(p_6)

#yaxis <- cowplot::get_y_axis(p_1)
grid.newpage()
grid.draw(legend)

p1n = p_1+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")
p2n = p_2+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")

full_hsapiens = grid.arrange( p1n  , p2n  , legend_h, ncol=3 )
ggsave(plot = full_hsapiens, width = 12, height = 10, dpi = 300, filename = "/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens_quality.pdf")

p_3
ggsave(plot = p_3, width = 7, height = 7, dpi = 300, filename = "/projects/btl_scratch/aafshinfard/projects/abyss2.5/osativa_quality.pdf")



legend <- cowplot::get_legend(p_4)
p4n = p_4+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")
p5n = p_5+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")
p6n = p_6+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")

full_ecoli = grid.arrange( p4n  , p5n, p6n  , legend_e, ncol=4 )
ggsave(plot = full_ecoli, width = 18, height = 10, dpi = 300, filename = "/projects/btl_scratch/aafshinfard/projects/abyss2.5/ecoli_quality.pdf")


xlimits = c(0,1000)
xbreaks = seq(0, 5000, by = 500)
xbreaks_l = xbreaks

ylimits2 = c(60000,90000)
y_breaks2=seq(60000,90000, by = 10000)
y_breaks_l2=c("60k","70k","80k","90k")

p_1 = plotter1(df_1$dataset[1], df_1, xlimits, ylimits2, y_breaks = y_breaks2, y_breaks_labels = y_breaks_l2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)
p_2 = plotter1(df_2$dataset[1], df_2, xlimits, ylimits2, y_breaks = y_breaks2, y_breaks_labels = y_breaks_l2, x_breaks = xbreaks, x_breaks_labels = xbreaks_l)

p1n = p_1+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")
p2n = p_2+theme(legend.position = "none")+rremove("y.text")+rremove("ylab")

full_hsapiens = grid.arrange( p1n  , p2n  , legend_h, ncol=3 )
ggsave(plot = full_hsapiens, width = 12, height = 10, dpi = 300, filename = "/projects/btl_scratch/aafshinfard/projects/abyss2.5/hsapiens_quality_cropped.pdf")













############################______________

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
                       values = c("dodgerblue2","darkorange1","mediumorchid3","turquoise3","black","grey")) +
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



