library(ggplot2)
library(PtProcess)
library(gtable)
library(grid)
library(gridExtra)

plottera <- function(experiment, data, xlimits=0, ylimits=0, breaks_count=0,
                     x_breaks=c(0,300,600,900,1200,1500,1800,2100,2400,2700,3000,3300),
                     x_breaks_labels=c("0","5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55" ),
                     y_breaks=c(0,50,100,150,200,250, 300, 350,400 ),
                     y_breaks_labels=c(0,50,100,150,200,250, 300, 350,400 )){
  if(xlimits==0){
    xlimits=c(0,3300)
  }
  if(ylimits==0){
    ylimits=c(0,400)
  }
  range50 <- ggplot(data, aes("time", `mem`, 
                              color = tool, 
                              shape = tool,#Sample,
                              fill = tool)) + #Algorithm)) 
    scale_color_manual(#name = "Algorithm", #"Tool", 
                       #values = c("black","deepskyblue1","seagreen2","goldenrod1")) + #indianred2
                       values = c("black","mediumorchid3","black","turquoise3","goldenrod1")) +
    scale_fill_manual(values = c("black","mediumorchid3","mediumorchid3","turquoise3","goldenrod1")) +
    scale_shape_manual(values = c(1, 19, 21, 19, 19))+
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 18, colour = "black"),
      legend.title = element_text(size=15),
      legend.text = element_text(size=14)
    ) +
    xlab("Wall clock time (hours)") + ylab("Peak memory (GB)") +
    scale_x_continuous(limits = xlimits,
                       breaks=x_breaks, labels=x_breaks_labels)+
    scale_y_continuous(limits = ylimits,
                       #breaks=c(0,5e6,10e6,15e6,20e6,25e6,30e6,40e6,50e6,60e6,70e6,80e6 ),
                       breaks=y_breaks,
                       #labels=c("0","5 Mb","10 Mb","15 Mb","20 Mb","25 Mb","30 Mb","40 Mb",
                       #        "50 Mb","60 Mb","70 Mb","80 Mb") )
                       labels=y_breaks_labels )
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
  return (frange50)
}


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

