rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test")
library(ggplot2)

source('https://raw.githubusercontent.com/dchakro/ggplot_themes/master/DC_theme_generator.R')
# source('https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/ggplotBreaks.R')
customtheme <- DC_theme_generator(type='L')

# rm(list=ls()[!ls() %in% c("DC_theme_generator","ggplotBreaks")])
rm(list=ls()[!ls() %in% c("DC_theme_generator")])
#-----------
# base_V_stringi
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
customtheme <- DC_theme_generator(type='L')
ggplot(data = DF, aes(x = group, y = time,fill=expr,group = interaction(group,expr)))+geom_boxplot(outlier.colour = NA,size=1,fill="white",aes(color=expr))+geom_point(pch=21,,size=2,position = position_jitterdodge())+customtheme+ylab("Time (µs)")+xlab("Number of observations")+facet_wrap(~group,nrow = 1,drop=T,scales = "free")+ggtitle("base vs stringi")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/baseVstringi.svg",height = 3,width = 10)

results <- readRDS("results/results_base_V_stringi.RDS")
results$size <- as.character(results$size)
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
ggplot(data = results, aes(x = size, y = time, color = expr, group = expr))+geom_point(size=2)+geom_line(size=1)+geom_errorbar(aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+ggtitle("base vs stringi")
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/baseVstringi_2.svg",height = 5,width = 5)

