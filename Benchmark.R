rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test/subset/")
library(microbenchmark)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

file.prefix <- c("11", "101", "1001", "10001", "100001", "1000001")
Number.of.alternatives <- 3 # base, readr, vroom
DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]
for(f in file.prefix){
  file.name <- paste0(f,"_d.tsv")
  bmark <- microbenchmark(
    "base" = {
      dat <- base::read.table(file = file.name,header = T,sep = "\t",as.is = T,stringsAsFactors = T)
      rm(dat)
    },
    "readr" = {
      dat <- readr::read_delim(file = file.name,delim = "\t",progress=F)
      rm(dat)
    },
    "vroom" = {
      dat <- vroom::vroom(file = file.name,delim = "\t",progress = F)
      rm(dat)
    }
    ,times = 5)
    gc()
    saveRDS(bmark,file = paste0("../bmark/bmark_reading_",f,".RDS"))
    results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
    results$size <- rep(f,length(results[,1]))
    DF <- rbind.data.frame(DF,results)
    rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_reading_",f,".RDS"))
