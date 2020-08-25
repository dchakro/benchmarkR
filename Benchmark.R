rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test/subset/")
# library(microbenchmark)
# library(bench)






bmark <- microbenchmark(
  "base" = {
    dat <- read.table(file = "test.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
    rm(dat);gc()
  },
  "readr" = {
    dat <- readr::read_delim(file = "test.tsv",delim = "\t",progress=F)
    rm(dat);gc()
  },
  "vroom" = {
    dat <- vroom::vroom(file = "test.tsv",delim = "\t",progress = F)
    rm(dat);gc()
  }
,times = 10)

bmark


bmark <- bench::mark(
  "base" = {
  dat <- read.table(file = "test.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
  rm(dat);gc()
},
"readr" = {
  dat <- readr::read_delim(file = "test.tsv",delim = "\t",progress=F)
  rm(dat);gc()
},
"vroom" = {
  dat <- vroom::vroom(file = "test.tsv",delim = "\t",progress = F)
  rm(dat);gc()
})

