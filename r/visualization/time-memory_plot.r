library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

plottera <- function(experiment, data, xlimits=0, ylimits=0, breaks_count=0,
                     x_breaks=c(0,300,600,900,1200,1500,1800,2100,2400,2700,3000,3300),
                     x_breaks_labels=c("0","5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55" ),
                     y_breaks=c(0,50,100,150,200,250, 300, 350,400 ),
                     y_breaks_labels=c(0,50,100,150,200,250, 300, 350,400 ),
                     annotate=TRUE){
  if(xlimits==0 && length(xlimits)<2){
    xlimits=c(min(data$time)/2 , max(data$time)*1.3)
  }
  if(ylimits==0 && length(ylimits)<2){
    ylimits=c(min(data$mem)/2, max(data$mem)*1.3)
  }
  range50 <- ggplot(data, aes("time", `mem`, 
                              color = tool,
                              label=tool,
                              #shape = tool,#Sample,
                              fill = tool)) + #Algorithm)) 
    scale_color_manual(#name = "Algorithm", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
      breaks= data$tool,                 
      values = data$color)+#c("mediumorchid3","goldenrod1","black")) +
    scale_fill_manual(breaks= data$tool,                 
                      values = data$color) +
    #scale_shape_manual(values = c(1, 19, 21, 19, 19))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Wall clock time (Minutes)") + ylab("Peak memory (GB)")+
    scale_x_continuous(limits =xlimits)+#,
    #                   breaks=x_breaks, labels=x_breaks_labels)+
    scale_y_continuous(limits = ylimits)
    #                   #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
    #                   breaks=y_breaks,
    #                   #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
    #                   #        "50 Mb","60 Mb","70 Mb","80 Mb") )
    #                   labels=y_breaks_labels )
  #labels=c("0","5","10","15","20","25","30","40", "50","60","70","80") )
  #return (range50)
  frange50 <- range50 + geom_point(aes(x=time, y= mem), size=7, stroke = 1) + ggtitle(experiment) +
    theme(
      panel.grid.major.y = element_line(colour="gray", size=0.6, linetype=1,
                                        lineend="round"),  # "round", "butt" or "square"
      panel.grid.major.x = element_line( size=0.5, linetype=1,
                                         lineend="round"),
      panel.grid.minor.y = element_line( size=0.5 ),
      panel.grid.minor.x = element_blank(),
    )
  if (annotate==TRUE){
    frange50 <- 
      frange50 + geom_text(data=data, mapping=aes(x=time, y=mem, label=tool), size=4, angle=0, vjust=-1.5, hjust=+0.15)
  }
  return (frange50)
}

ad_sum="/projects/btl_scratch/aafshinfard/projects/abyss2.5/ecoli/run-summary.tsv"
ad_sum="/projects/btl/aafshinfard/github/scripts/temp/runtimes.tsv"



df_temp = read.table(ad_sum, header = FALSE, sep = "|", colClasses = c("character","character","character","character","character","character"))
df = df_temp[,c(2,3,4,5,6,7)]
df2=df

colnames(df2)=c("group","tool","time","mem","color", "order")

df2$group=str_trim(df2$group)
df2$tool=
df2$time=as.numeric(str_trim(table$V5))
df2$mem=as.numeric(str_trim(table$V5))
df2$color=
df2$order

library(gridExtra)

#p1=plottera(experiment, df2[df2[,1]==" Ecoli 2x100",])
legend_e <- cowplot::get_legend(p1)
p1=plottera(" Ecoli 2x100", df2[df2[,1]==" Ecoli 2x100",],annotate = FALSE)+theme(legend.position = "none")
p2=plottera(" Ecoli 2x150", df2[df2[,1]==" Ecoli 2x150",])+theme(legend.position = "none")
p3=plottera(" Ecoli 2x301", df2[df2[,1]==" Ecoli 2x301",])+theme(legend.position = "none")


grid.arrange(p1, p2, p3, legend_e, ncol=4)


xbreaks = seq(0, 5000, by = 500)

x_breaks=seq(180, 900, by = 60)
x_breaks_labels=x_breaks
y_breaks=seq(0, 1800, by = 200)
y_breaks_labels=y_breaks


p4=plottera(experiment, df2[df2[,1]==" na12878 ",], xlimits = c(0,1800), ylimits = c(0,1800), x_breaks=x_breaks, x_breaks_labels = x_breaks_labels, y_breaks = y_breaks, y_breaks_labels = y_breaks )
legend_h <- cowplot::get_legend(p4)
p4=plottera(experiment, df2[df2[,1]==" na12878 ",], xlimits = c(0,1800), ylimits = c(0,1800), x_breaks=x_breaks, x_breaks_labels = x_breaks_labels, y_breaks = y_breaks, y_breaks_labels = y_breaks )+theme(legend.position = "none")
p5=plottera(experiment, df2[df2[,1]==" na24631 ",], xlimits = c(0,1800), ylimits = c(0,1800), x_breaks=x_breaks, x_breaks_labels = x_breaks_labels, y_breaks = y_breaks, y_breaks_labels = y_breaks )+theme(legend.position = "none")
grid.arrange(p4, p5, legend_h, ncol=3)




xbreaks=seq(60, 360, by = 60)
x_breaks=seq(60, 360, by = 60)
x_breaks_labels=x_breaks
y_breaks=seq(0, 700, by = 100)
y_breaks_labels=y_breaks

p6=plottera(experiment, df2[df2[,1]==" OSativa ",], xlimits = c(0,360), ylimits = c(0,700), x_breaks=x_breaks, x_breaks_labels = x_breaks_labels, y_breaks = y_breaks, y_breaks_labels = y_breaks )

#read.table(ad_sum, header = FALSE, sep = "|", skip = 0, as.is = c(2,3,4,5,6,7,8,9,10), colClasses = c("character","character","numeric","numeric","numeric","numeric","character","character","character","character","character"))











#########################################################
tool = c(
  "Physlr (physical-map)","Physlr (scaffolds)", "Physlr (complete)", "ARCS", "SLR-SS"
)

time = c(
  820, 596, 1405, 444, 3070
)
mem = c(
  360, 144, 360, 15, 331
)
order = c(
  "1","2","3","4", "5"
)
grp = c(
  "1","1","1","2", "3"
)

df = data.frame(
  "tool"=tool,
  "time"=time,
  "mem"=mem,
  "order"=order,
  "grp"=grp
)

df$tool <- factor(df$tool, levels=unique(df$tool[order(df$order)] ))
#aq$Algorithm <- factor(aq$Algorithm, levels = unique(aq$Algorithm[order(aq$alg.order)]))

experiment="NA12878 PE+MPET"
plottera(experiment, df)


ggplot(df,aes(x=time, y=mem))+geom_point()+
  
  
  geom_hline(yintercept=c(2,100), linetype='dashed', col = 'red')+annotate("text", color="red", x = xs, y =c(2,100), label = c("mult=2","mult=100"), vjust = c(-0.5,-0.5),hjust =c(-0.02,-0.02), size=5)+
  ylim(c(0,200))+xlim(c(xs,xe))+
  xlab("K-mer position in the read")+ylab("K-mer multiplicity")+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  )

