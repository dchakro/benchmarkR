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
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+ggtitle("Populating a vector")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))
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
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("Populating a vector")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/assignVconcat_box.svg",height = 4,width = 10)

#-----------
# base_V_stringi
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

results <- readRDS("results/results_base_V_stringi.RDS")
results$size <- as.character(results$size)
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr, group = interaction(expr,size)))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+ggtitle("String operations")+facet_wrap(~size,nrow = 1,drop=T,scales = "free")+geom_text(position=position_dodge(width=0.9))
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
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("String operations")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/baseVstringi_box.svg",height = 4,width = 10)

#------------
# brackets
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- readRDS("bmark/bmark_brackets.RDS")
DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+ggtitle("Brackets")
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
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("fixed & unlist")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/fixed_and_unlist_box.svg",height = 4,width = 12)

#-----------
# for_V_apply
rm(list=ls()[!ls() %in% c("DC_theme_generator")])

DF <- readRDS("bmark/bmark_for_V_apply.RDS")
DF$time <- DF$time/1e+06
customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)

ggplot(data = DF[DF$expr!="smart",], aes(x = expr, y = time))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+ggtitle("for vs apply")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/for_v_apply_box.svg",height = 5,width = 3)

ggplot(data = DF, aes(x = expr, y = time))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (µs)")+xlab("size")+ggtitle("for vs apply s smart")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/for_v_apply_box2.svg",height = 5,width = 4)

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
ggplot(data = DF, aes(x = expr, y = time, group = interaction(group,expr)))+geom_boxplot(size=0.75,outlier.color = NA,aes(col = expr))+geom_point(position = position_jitterdodge(),aes(fill = expr),pch=21)+customtheme+ylab("Time (seconds)")+xlab("size")+facet_wrap(~group ,nrow = 1,drop=T,scales = "free")+ggtitle("forVforeach")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/forVforeach.svg",height = 4,width = 6)

#-----------------
## Vectorization
DF <- readRDS("bmark/bmark_vectorization.RDS")
results <- readRDS("results/results_vectorization.RDS")
DF$time <- DF$time/1e+06
ggplot(data = DF, aes(x = expr, y = time, fill = expr))+stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,geom = "crossbar",color="black",width = 0.25)+geom_dotplot(binwidth = 100,binaxis = "y",dotsize=1,stackdir = "center",position = "dodge")+customtheme+ylab("Time (µs)")+xlab("Method")+annotate("text",x=1,y=150,label="9.77 µs")+annotate("text",x=2,y=4230,label="3881 µs")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/vectorization.svg",height = 5,width = 4)


