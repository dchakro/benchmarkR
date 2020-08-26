rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test/subset/")
library(microbenchmark)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

## Reading TSV
# base, readr, vroom
test.name <- "readingTSV"
file.prefix <- c("11", "101", "1001", "10001", "100001", "1000001")
DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]
for(f in file.prefix){
  file.name <- paste0(f,"_d.tsv")
  bmark <- microbenchmark(
    "base" = {
      dat <- utils::read.table(file = file.name,header = T,sep = "\t",as.is = T,stringsAsFactors = T)
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
    saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",f,".RDS"))
    results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
    results$size <- rep(f,length(results[,1]))
    DF <- rbind.data.frame(DF,results)
    rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_",test.name,"_",f,".RDS"))
rm(list=ls())
gc()

# Writing TSV
# base, readr, vroom then saveRDS
test.name <- "saveRDS"
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
file.prefix <- c("11", "101", "1001", "10001", "100001", "1000001")
DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]
for(f in file.prefix){
  file.name <- paste0(f,"_d.tsv")
  
  dat <- readr::read_delim(file = file.name,delim = "\t",progress=F)
  bmark_readr <- microbenchmark( "readr" = {
      saveRDS(dat,file = "../tmp.dat.RDS")
    },times = 5)
  rm(dat)
  
  dat <- utils::read.table(file = file.name,header = T,sep = "\t",as.is = T,stringsAsFactors = T)
  bmark_base <- microbenchmark( "base" = {
    saveRDS(dat,file = "../tmp.dat.RDS")
  },times = 5)
  rm(dat)
  
  dat <- vroom::vroom(file = file.name,delim = "\t",progress = F)
  bmark_vroom <- microbenchmark( "vroom" = {
      saveRDS(dat,file = "../tmp.dat.RDS")
    },times = 5)
  rm(dat)
  
  bmark <- rbind.data.frame(bmark_base,bmark_readr,bmark_vroom) 
  rm(bmark_base,bmark_readr,bmark_vroom)
  gc()
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",f,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(f,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_",test.name,"_",f,".RDS"))
rm(list=ls())
gc()

DF <- readRDS("../results/results_reading_1000001.RDS")
