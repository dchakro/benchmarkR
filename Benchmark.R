rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test/subset/")
library(microbenchmark)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

#-----------------
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
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#-----------------------
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
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#--------------------------------
# Vectorized functions: parenthesis vs curly
test.name <- "brackets"
dat <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
bmark <- microbenchmark("parenthesis" = {
  pM <- rep(NA,length(dat$Gene.name))
  for(i in seq_along(dat$Gene.name)){
    pM[i] <- ((dat$ID_tumour[i]+dat$ID_sample[i]+dat$MUTATION_ID[i])/3)
  }
 },
"curly" = {
  cM <- rep(NA,length(dat$Gene.name))
  for(i in seq_along(dat$Gene.name)){
    cM[i] <- {{dat$ID_tumour[i]+dat$ID_sample[i]+dat$MUTATION_ID[i]}/3}
  }
}, times = 5 )
identical(cM,pM)
saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,".RDS"))
rm(cM,pM,i)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
DF <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#----------------------------
# Vectorized functions: mean vs rowmeans
test.name <- "Vectorize-mean"
dat <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
bmark <- microbenchmark("rowMeans" = {
  rM <- rowMeans(dat[,c("ID_sample","ID_tumour","MUTATION_ID")])
},
"mean" = {
  M <- rep(NA,length(dat$Gene.name))
  for(i in seq_along(dat$Gene.name)){
    M[i] <- {{dat$ID_tumour[i]+dat$ID_sample[i]+dat$MUTATION_ID[i]}/3}
  }
}, times = 5 )
identical(M,rM)
saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,".RDS"))
rm(M,rM,i)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
DF <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#----------------------
# Vectorization
test.name <- "vectorization"
dat <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
bmark <- microbenchmark("vectorize" = {
  calc_vec <- {{dat$ID_tumour+dat$ID_sample}/dat$HGNC.ID}
},
  "for" = {
    calc_for <- rep(NA,length(dat$Gene.name))
    for(i in seq_along(dat$Gene.name)){
      calc_for[i] <- {{dat$ID_tumour[i]+dat$ID_sample[i]}/dat$HGNC.ID[i]}
    }
  }, times = 5 )
identical(calc_for,calc_vec)
saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,".RDS"))
rm(calc_for,i,calc_vec)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
DF <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#----------------------------
# unlist & fixed
test.name <- "fixed_and_unlist"
dat <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

genomePosition <- dat$Mutation.genome.position
genomePos <- genomePosition[1:i]

source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(1,10,100,1000,10000,100000,1000000)){
  genomePos <- dat$Mutation.genome.position[1:i]
  bmark <- microbenchmark("regular" = {
    res_1 <-  base::substring(text = genomePos,first = {unlist(base::gregexpr(pattern = ":",text = genomePos))+1})
  }, "smart"={
    res_2 <- base::substring(text = genomePos,first = {unlist(base::gregexpr(pattern = ":",text = genomePos, fixed = T),use.names = F)+1})
  }, times = 5)
  # print(identical(res_1,res_2))
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))

rm(list=ls())
gc()


#-----------------------------
# for vs apply
test.name <- "for_V_apply"
dat <- utils::read.table(file = "100001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

genomePosition <- dat$Mutation.genome.position

bmark <- microbenchmark("for" = {
  res_for <- rep(NA,length(genomePosition))
  for(i in seq_along(genomePosition)){
    genomePos <- base::substring(text = genomePosition[i],first = base::gregexpr(pattern = ":",text = genomePosition[i], fixed = T)[[1]][1]+1)
    minus <- base::gregexpr(pattern = "-",text = genomePos, fixed = T)[[1]][1]
    res_for[i] <- as.integer(substring(text = genomePos,first = 1,last = minus-1))-as.integer(substring(text = genomePos,first = minus+1))
  }
},
"apply" = {
  # define function
  find_length <- function(genomePos = NULL){
    genomePos <- base::substring(text = genomePos,first = base::gregexpr(pattern = ":",text = genomePos, fixed = T)[[1]][1]+1)
    minus <- base::gregexpr(pattern = "-",text = genomePos, fixed = T)[[1]][1]
    return(as.integer(substring(text = genomePos,first = 1,last = minus-1))-as.integer(substring(text = genomePos,first = minus+1)))
  }
  res_apply <- sapply(X = genomePosition,FUN = find_length,USE.NAMES = F)
},
"smart" = {
  # define function that iterates by itself
  find_length_base <- function(genomePos = NULL){
    genomePos <- base::substring(text = genomePos,first = {unlist(base::gregexpr(pattern = ":",text = genomePos, fixed = T),use.names = F)+1})
    minus <- unlist(base::gregexpr(pattern = "-",text = genomePos, fixed = T),use.names = F)
    return(as.integer(substring(text = genomePos,first = 1,last = minus-1))-as.integer(substring(text = genomePos,first = minus+1)))
  }
  res_smart <- find_length_base(genomePos = genomePosition)
}, times = 5 )

identical(res_apply,res_for)
identical(res_apply,res_smart)
saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,".RDS"))
rm(res_for,res_apply,res_smart,i,genomePos,minus,find_length,find_length_base,genomePosition)

source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
DF <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#-------------------------------------
# base vs stringi
test.name <- "base_V_stringi"
dat <- utils::read.table(file = "100001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

find_length_stringi <- function(genomePos = NULL){
  genomePos <- stringi::stri_sub(str = genomePos,from =  stringi::stri_locate_first_fixed(str = genomePos,pattern = ":")[,'start']+1,to = -1)
  minus <- stringi::stri_locate_first_fixed(str = genomePos,pattern = "-")[,'start']
  return(as.integer(stringi::stri_sub(str = genomePos, from=1, to = minus-1))-as.integer(stringi::stri_sub(str = genomePos, from=minus+1, to = -1)))
}

find_length_base <- function(genomePos = NULL){
  genomePos <- base::substring(text = genomePos,first = {unlist(base::gregexpr(pattern = ":",text = genomePos, fixed = T),use.names = F)+1})
  minus <- unlist(base::gregexpr(pattern = "-",text = genomePos, fixed = T),use.names = F)
  return(as.integer(substring(text = genomePos,first = 1,last = minus-1))-as.integer(substring(text = genomePos,first = minus+1)))
}

source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(1,10,100,1000,10000,100000)){
  genomePosition <- dat$Mutation.genome.position[1:i]
  bmark <- microbenchmark("base" = {
    res_b <- find_length_base(genomePos = genomePosition)
  }, "stringi"={
    res_s <- find_length_stringi(genomePos = genomePosition)
  }, times = 5)
  # print(identical(res_b,res_s))
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

#-----------------------------
# parallel processing
# for vs foreach
library(doParallel)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
test.name <- "forVforeach"
dat <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

dat <- dat[-grep("_ENST",dat$Gene.name,fixed = T),]
dat$Mutation.AA <- gsub("p.","",dat$Mutation.AA,fixed = T)
dat <- dat[-grep("?",dat$Mutation.AA,fixed = T),]
dat <- dat[-grep("=",dat$Mutation.AA,fixed = T),]
# dim(dat) 106450     40

dat$mutID <- paste(dat$Gene.name,dat$Mutation.AA,sep="=")
mutations <- unique(dat$mutID) # 79972

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(100,1000,10000)){
  bmark <- microbenchmark("foreach"= {
    myCluster <- makeCluster(4, type = "FORK",useXDR=F,.combine=cbind)
  print(myCluster)
  registerDoParallel(myCluster)
  results_1 <- foreach(mut = mutations[1:i],.combine = cbind,.inorder = T) %dopar% {
    var1 <-  plyr::count(dat[dat$mutID==mut,],"Primary.site")
    var1 <- var1[order(var1$freq,decreasing = T),]
    return(list(mut,sum(var1$freq),stringi::stri_paste(var1$Primary.site,":",var1$freq,collapse=',')))
  }
  stopCluster(myCluster) },
  "foreach.fast"= {
    myCluster <- makeCluster(4, type = "FORK",useXDR=F,.combine=cbind)
    print(myCluster)
    registerDoParallel(myCluster)
    results_2 <- foreach(mut = mutations[1:i],.combine = cbind,.inorder = F) %dopar% {
      var1 <-  plyr::count(dat[dat$mutID==mut,],"Primary.site")
      var1 <- var1[order(var1$freq,decreasing = T),]
      return(list(mut,sum(var1$freq),stringi::stri_paste(var1$Primary.site,":",var1$freq,collapse=',')))
    }
    stopCluster(myCluster) },
  "for"={
    results_3 <- list()
    for(mut in mutations[1:i]){
      var1 <-  plyr::count(dat[dat$mutID==mut,],"Primary.site")
      var1 <- var1[order(var1$freq,decreasing = T),]
      results_3 <- cbind(results_3,list(mut,sum(var1$freq),stringi::stri_paste(var1$Primary.site,":",var1$freq,collapse=',')))}
    },times = 5)
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()

# lapply vs mclapply vs vs parLapply
library(doParallel)
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
test.name <- "lapplyVmclapplyVparLapply"
dat <- utils::read.table(file = "100001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(10,100,1000,10000,length(unique(dat$MUTATION_ID)))){
  bmark <- microbenchmark(
    "lapply" ={
      res_l <- lapply(X = unique(dat$Gene.name), FUN = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T))
    },
    "parLapply" ={
      myCluster <- makeCluster(4, type = "FORK",useXDR=F,.combine=cbind)
      print(myCluster)
      registerDoParallel(myCluster)
      res_par <- parLapply(cl = myCluster,X = unique(dat$Gene.name), fun = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T))
      stopCluster(myCluster)
    }, 
    "mclapply" = {
      res_mcl <- mclapply(X = unique(dat$Gene.name), FUN = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T),mc.cores = parallel::detectCores())
    }, times = 5)
  # print(identical(res_l,res_par))
  # print(identical(res_mcl,res_par))
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
