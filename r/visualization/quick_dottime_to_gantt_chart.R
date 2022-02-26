library("dplyr")
library("ggplot2")
library("stringr")

#path <- params$
path <-"/projects/btl_scratch/aafshinfard/projects/abyss3/human3"
m <- matrix(ncol = 9)
df = data.frame(m)
colnames(df)=c("step",
               "total_time_min",
               "time_hr",
               "time_min",
               "time_sec",
               "mem",
               "threads",
               "p-cpu",
               "pl")

if (substr(path,nchar(path),nchar(path)) != "/")
  path <- paste(path,"/",sep="")
ldir <-  normalizePath(path)

cat(ldir)
finf <- file.info(dir(path = ldir, full.names = TRUE), extra_cols = FALSE)
sorted_finf <- finf[with(finf, order(as.POSIXct(ctime))), ]
sorted_files <- rownames(sorted_finf)
sorted_time_files <- sorted_files[substr(sorted_files,
                                         nchar(sorted_files)-4,nchar(sorted_files)) == ".time"]

i=2
good = TRUE
for (i in 1:length(sorted_time_files))
{
  lines_of_file <- readLines(sorted_time_files[i])
  if(FALSE){
    if (length(lines_of_file) != 23)
    {
      cat(paste("The time-file is corrupted: \n", sorted_time_files[i]))
      next
    }
    # Exit status
    exit_line_no = grep('Exit', lines_of_file)
    exit_status <- substr(lines_of_file[exit_line_no],
                          nchar(lines_of_file[exit_line_no]),
                          nchar(lines_of_file[exit_line_no]))
    if (exit_status != "0")
    {
      cat("The job you are profiling did not exit normally, relative file:\n")
      print(sorted_time_files[i])
      next
    }
  }
  ## percent cpu
  pcpu_line_no = grep('Percent', lines_of_file)
  if (length(pcpu_line_no) != 0){
    pcpu_line <- lines_of_file[pcpu_line_no]
    print(pcpu_line)
    df[i,"p-cpu"]=strtoi(sapply(strsplit(sapply(strsplit(pcpu_line, split = " "), tail, 1), split="%"), tail, 1))
    print(df[i,"p-cpu"])
  }else{
    df[i,"p-cpu"]=0
  }
  ## Time
  time_line_no = grep('Elapsed', lines_of_file)
  time_line <- lines_of_file[time_line_no]
  time = str_match(
    time_line,
    regex(
      '(?<=or m:ss[[:punct:]][[:punct:]]\ ).*',
      ignore.case = TRUE,
      perl = TRUE
    )
  )[1, 1]
  hms = str_match(time, regex('([:digit:]+)\\:([:digit:]+)\\:([:digit:]+)'))
  msms = str_match(time, regex('([:digit:]+)\\:([:digit:]+)\\.([:digit:]+)'))
  if ( sum(is.na(hms)) == 0 )
  {
    df[i, "time_sec"] = hms[1,4]
    df[i, "time_min"] = hms[1,3]
    df[i, "time_hr"] = hms[1,2]
  } else if ( sum(is.na(msms)) == 0 )
  {
    df[i, "time_sec"] = msms[1,3]
    df[i, "time_min"] = msms[1,2]
    df[i, "time_hr"] = 0
  } else
  {
    cat(paste("Unexpected time format."))
  }
  if (df[i, "time_hr"] == 0 && df[i, "time_min"] == 0)
  {
    df[i, "time_min"] = 1
  }
  df[i, "total_time_min"] = strtoi(df[i, "time_hr"], base = 10L) * 60 + strtoi(df[i, "time_min"], base = 10L)
  ## Memory
  mem_line_no = grep('Maximum', lines_of_file)
  mem_line <- lines_of_file[mem_line_no]
  df[i, "mem"] = strtoi(str_match(lines_of_file[mem_line_no], regex('(?<=:\ ).*')))
  ## Name and programming language
  py_pattern = str_match(lines_of_file[1], regex('/bin/ABySS 3 // GoldRush\ \\S*'))
  if (!is.na(py_pattern[1, 1]))
  {
    #name_loci = str_locate_all(lines_of_file[1], "bin/GoldRush")
    df[i, "step"] = str_match(py_pattern,
                              regex('(?<=\ ).*',
                                    ignore.case = TRUE, perl =
                                      TRUE))
    df[i, "pl"] = "python"
  } else
  {
    cpp_pattern = str_match(lines_of_file[1], regex('/src/GoldRush-\\S*'))
    if (!is.na(cpp_pattern[1, 1]))
    {
      df[i, "step"] = str_match(cpp_pattern,
                                regex('(?<=-).*',
                                      ignore.case = TRUE, perl =
                                        TRUE))
      df[i, "pl"] = "c/c++"
    } else
    {
      pattern = str_match(lines_of_file[1], regex('being timed:\ \"\\S*'))
      if (FALSE)#if (!is.na(pattern[1, 1]))
      {
        df[i, "step"] = str_match(pattern,
                                  regex(
                                    '(?<=being timed:\ \").*',
                                    ignore.case = TRUE,
                                    perl = TRUE
                                  ))
        df[i, "pl"] = "third-party"
      } else
      {
        cat("Unknown command pattern:\n")
        print(lines_of_file[1])
        
        df[i, "step"] = sapply(strsplit(strsplit(sapply(strsplit(sorted_time_files[i], split = "/"), tail, 1), split =".time")[[1]], split = "reads.k25."), tail, 1)
        df[i, "pl"] = "unknown"
      }
    }
  }
}
i=2
df


#df=df0
df0=df
df2=df0
#df=df0
df=df[df[,"step"]!="racon.ntlink",]
df[df[,"step"]=="wp2","step"]="work_package_2_Johnathan"
df[df[,"step"]=="headers.trimmed.k25.scoresorted.first_scoring.fa","step"]="k25.scoresorted.first_scoring_Vlad"
df[df[,"step"]=="headers.trimmed.fa","step"]="trimmed_Vlad"
df[df[,"step"]=="readscores.first_scoring.tsv","step"]="readscores.first_scoring_Vlad"
df[df[,"step"]=="cbf","step"]="cBF_Vlad"
df[df[,"step"]=="headers.fa","step"]="headers_Vlad"
df[df[,"step"]=="racon","step"]="racon_Lauren"

df[c(1,2,3,4,5,6),"pl"]="C++"
df[c(7,8,9,10),"pl"]="other"

df[6,"total_time_min"]=535
df[6,"time_hr"]=8
df[6,"time_min"]=55
df[6,"time_sec"]=0
df[6,"mem"]=75.2*1024*1024

df2$step="WP2 - generate 1 golden path"
df2$total_time_min=195.43
df2$time_hr=3
df2$time_min=15
df2$time_sec=26
df2$mem=3569840
df2$pl="C++"

df[1,]=df2

df2=df0  
df2$step="WP2 - generate 2 golden paths"
df2$total_time_min=266.27
df2$time_hr=4
df2$time_min=26
df2$time_sec=16
df2$mem=6521960
df2$pl="C++"
df[2,]=df2

df2=df0
df2$step="WP2 - generate 3 golden paths"
df2$total_time_min=370.35
df2$time_hr=6
df2$time_min=10
df2$time_sec=21
df2$mem=8015040
df2$pl="C++"
df[3,]=df2

stacked=TRUE
df_temp = df
df = df_temp

df$`p-cpu`[6]=4038
df$`p-cpu`=round(df$`p-cpu`/10)/10
df$threads=c(48,48,1,1,1,48,48,8,4,4)
  
if (dim(df)[1] == 0) {
  cat("There is no *.time file to extract results from!")
  good = FALSE
} else if (sum(rowSums(is.na(df[, ])) > 0) == dim(df)[1]) {
  cat("No correct *.time file to extract results from!")
  good = FALSE
} else{
  unknown_steps = which(is.na(df[, 1])) # steps with unknown name
  if (length(unknown_steps) > 0) {
    cat("Following file(s) will be skipped due to having no step-name\n")
    print(sorted_time_files[unknown_steps])
    df = df[!is.na(df[, 1]),]
  }
  unknown_values = array() # steps with any unknown value in any column
  for (i in 1:dim(df)[1]) {
    if (sum(is.na(df[i,])) > 0) {
      unknown_values[length(unknown_values)[1] + 1] = i
    }
  }
  if (length(unknown_values) > 1) {
    unknown_values = unknown_values[2:length(unknown_values)]
    cat("Following file(s) will be excluded due to having NA values")
    print(print(sorted_time_files[unknown_values]))
    print(df[unknown_values,])
    df = df[!unknown_values,]
  }
  ## intervals:
  df[, "start"] = 0
  for (i in 1:dim(df)[1]) {
    if (stacked==TRUE){
      if (i > 1)
        df[i, "start"] = sum(df[1:(i - 1), "total_time_min"])
      df[i, "end"] = df[i, "start"] + df[i, "total_time_min"]
    } else {
      df[i, "end"] = df[i, "total_time_min"]
    }
  }
  duplicates = duplicated(df[, 1])
  if (sum(duplicates) > 1) {
    cat("Following rows will be deleted from the report as they're duplicated steps:")
    print(df[duplicates, c(1, 3, 4, 6, 7)])
    df = df[!duplicates,]
  }
  df$step <- factor(df$step, levels = df$step[order(df$start)])
  df$mem = ceiling(df$mem * 10 / (1000000)) / 10.0
  linerage_size = 7
  if (dim(df)[1] > 15)
    linerage_size = 6
  if (dim(df)[1] > 20)
    linerage_size = 5
  p_time <- ggplot(df, aes(x = step,
                           y = end,
                           colour = pl))
  p_time <- p_time +
    geom_linerange(aes(ymin = df$start, ymax = df$end), size = linerage_size) +
    coord_flip() +
    theme(text = element_text(size = 16)) +
    labs(
      title = "ABySS3/GoldRush - time Gantt chart",
      subtitle = paste("path:", ldir),
      caption = "GoldRush version: ?"
    ) +
    geom_hline(yintercept = c(df$end), linetype="dotted", 
               color = "black", size=0.3)+
    theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(color = "darkblue", size = 8),
      plot.caption = element_text(
        color = "darkgreen",
        face = "bold",
        size = 8
      )
    ) +
    theme(legend.title = element_blank()) +
    geom_text(
      label = sprintf(paste(
        df$time_hr, ":", df$time_min, ":", df$time_sec, sep = ""
      )),
      size = 4.5,
      hjust = -0.1,
      vjust = -0.1,
      check_overlap = T
    ) +
    geom_text(
      label = sprintf(paste(
        df$`p-cpu`, " of ", df$threads, " threads", sep = ""
      )),
      size = 3.9,
      hjust = -0.1,
      vjust = 1.5,
      check_overlap = T
    ) +
    scale_y_continuous(limits = c(0, max(df$end) * 1.1),breaks=seq(0, 6000, by = 60), labels = seq(0, 100, by = 1))+
    #scale_y_continuous(limits = c(0, df$end[dim(df)[1]] * 1.1),breaks=seq(0, 6000, by = 60), labels = seq(0, 100, by = 1))+
    #ylim(0, df$end[dim(df)[1]] * 1.1) +
    ylab("Time in hours") +
    xlab("GoldRush Step") +
    scale_color_manual(values = c("#008000", "#8E44AD", "#2E86C1", "#808080"))
  p_mem <- ggplot(df, aes(x = step,
                          y = mem,
                          colour = pl))
  p_mem <- p_mem +
    geom_linerange(aes(ymin = 0, ymax = df$mem), size = linerage_size) +
    coord_flip() +
    theme(text = element_text(size = 16)) +
    labs(
      title = "ABySS3/GoldRush - memory usage",
      subtitle = paste("path:", ldir),
      caption = "GoldRush version: ?"
    ) +
    theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(color = "darkblue", size = 8),
      plot.caption = element_text(
        color = "darkgreen",
        face = "bold",
        size = 8
      )
    ) +
    theme(legend.title = element_blank()) +
    geom_text(
      label = sprintf("%1.1f GB", df$mem),
      size = 4.5,
      hjust = -0.1,
      vjust = -0.1,
      check_overlap = T
    ) +
    ylim(0, max(df$mem) * 1.1) +
    ylab("Memory in GB") +
    xlab("GoldRush Step") +
    scale_color_manual(values = c("#008000", "#8E44AD", "#2E86C1", "#808080"))
}

df[,c(1,3,4,6,7)]


## Time:

```{r echo=FALSE, out.width = '100%'}
if (good)
  p_time
```

## Memory:
```{r echo=FALSE, out.width = '100%'}
if (good)
  p_mem
```
