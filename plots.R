rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test")
library(ggplot2)
source('https://raw.githubusercontent.com/dchakro/ggplot_themes/master/DC_theme_generator.R')

# source('https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/ggplotBreaks.R')
# rm(list=ls()[!ls() %in% c("DC_theme_generator","ggplotBreaks")])

#-----------------
## Assign vs concat
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_assignVconcat.RDS")
results$size <- as.character(results$size)
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Populating a vector")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/assignVconcat_bar.svg",height = 5,width = 14)


DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_assignVconcat_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
# DF$group <- as.integer(DF$group)
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("Populating a vector")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/assignVconcat_box.svg",height = 4,width = 10)

#-----------
# base_V_stringi
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_base_V_stringi.RDS")
results$size <- as.character(results$size)
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("String operations")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/baseVstringi_bar.svg",height = 5,width = 14)


DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_base_V_stringi_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
# DF$group <- as.integer(DF$group)
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("String operations")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/baseVstringi_box.svg",height = 4,width = 10)

#------------
# brackets
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- readRDS("bmark/bmark_brackets.RDS")
DF$time <- DF$time/1e+06
by(data = DF$time,DF$expr,mean)

customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Brackets")+expand_limits(y = 0)+annotate("text",x=1,y=8650,label="8452 µs")+annotate("text",x=2,y=8750,label="8618 µs")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/brackets_box.svg",height = 5,width = 4)

#----------
# fixed_and_unlist
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_fixed_and_unlist_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
# DF$group <- as.integer(DF$group)
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("fixed & unlist")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/fixed_and_unlist_box.svg",height = 4,width = 12)

#-----------
# for_V_apply
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_for_V_apply.RDS")

DF <- readRDS("bmark/bmark_for_V_apply.RDS")
DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)

ggplot(data = DF[DF$expr!="smart",], aes(x = expr, y = time))+geom_boxplot(size=0.5,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+ggtitle("for vs apply")+annotate("text",x=1,y=6050,label="5837 µs")+annotate("text",x=2,y=5900,label="5677 µs")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/for_v_apply_box.svg",height = 5,width = 4)

by(data = DF$time,DF$expr,mean)
ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.5,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("for vs apply s smart")+annotate("text",x=1,y=6050,label="5837 µs")+annotate("text",x=2,y=5900,label="5677 µs")+annotate("text",x=3,y=1200,label="943 µs")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/for_v_apply_box2.svg",height = 5,width = 5)

#-----------
# bmark_forVforeach
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_forVforeach_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
# DF$group <- as.integer(DF$group)
rm(tmp,testname,f)

DF$time <- DF$time/1e+09
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (seconds)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("forVforeach")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/forVforeach.svg",height = 4,width = 8)

#-----------
# lapplyVmclapplyVparLapply
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_lapplyVmclapplyVparLapply_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
DF$group <- as.character(format(as.integer(DF$group),scientific=F,trim = T))
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)

ggplot(data = DF, aes(x = group, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = group))+geom_point(position = position_jitterdodge(),aes(fill = group),pch=21)+customtheme+ylab("Time (µs)")+xlab("Size")+facet_wrap(~expr ,nrow = 1,drop=T,scales = "free")+ggtitle("String operations")+scale_y_continuous(limits = c(0,max(DF$time)))+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/lapply_family_box_1.svg",height = 4,width = 8)


ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("String operations")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/lapply_family_box_2.svg",height = 4,width = 8)

#------------
# parallel_saveRDS
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_parallel_saveRDS.RDS")
results$size <- as.character(format(as.integer(results$size)-1,scientific=F,trim = T))
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("parallel saveRDS")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/parallel_saveRDS_bar.svg",height = 5,width = 14)


DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_parallel_saveRDS_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
DF$group <- as.character(format(as.integer(DF$group)-1,scientific=F,trim = T))
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("parallel saveRDS")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/parallel_saveRDS_box.svg",height = 4,width = 12)

#---------
# readingTSV
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_readingTSV.RDS")
results$size <- as.character(format(as.integer(results$size)-1,scientific=F,trim = T))
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Reading files")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/reading_tsv_bar.svg",height = 5,width = 14)


DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_readingTSV_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
DF$group <- as.character(format(as.integer(DF$group)-1,scientific=F,trim = T))
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("Reading files")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/reading_tsv_box.svg",height = 4,width = 12)

#----------
# Saving RDS
# saveRDS
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_saveRDS.RDS")
results$size <- as.character(format(as.integer(results$size)-1,scientific=F,trim = T))
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Saving files")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/saveRDS_bar.svg",height = 5,width = 14)


DF <- data.frame(expr=NA,time=NA,group=NA)
DF <- DF[-1,]
testname <- "bmark_saveRDS_"
for (f in list.files(path = "bmark/",pattern = testname)){
  tmp <- readRDS(paste0("bmark/",f))
  tmp$group <- rep(f,length(tmp[,1]))
  DF <- rbind(DF,tmp)
}
DF$group <- gsub(testname,"",DF$group,fixed = T)
DF$group <- gsub(".RDS","",DF$group,fixed = T)
DF$group <- as.character(format(as.integer(DF$group)-1,scientific=F,trim = T))
rm(tmp,testname,f)

DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 1.25,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("Saving files")+expand_limits(y = 0)
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/saveRDS_box.svg",height = 4,width = 12)

#-----------------
# Vectorization
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_vectorization.RDS")
DF <- readRDS("bmark/bmark_vectorization.RDS")
DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45,legend = F)

by(data = DF$time,DF$expr,mean)
ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.5,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Vectorization vs for")+annotate("text",x=1,y=100,label="7.95 µs")+annotate("text",x=2,y=4000,label="3882 µs")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/vectorization.svg",height = 5,width = 3)

#----------
# Vectorization - means
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_Vectorize-mean.RDS")
DF <- readRDS("bmark/bmark_Vectorize-mean.RDS")
DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45,legend = F)

by(data = DF$time,DF$expr,mean)
ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.5,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(jitter.width = 2,seed = 21),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Vectorized mean")+annotate("text",x=1,y=300,label="31.3 µs")+annotate("text",x=2,y=9000,label="8970 µs")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/Vectorization-mean.svg",height = 5,width = 3)
