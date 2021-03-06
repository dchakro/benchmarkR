rm(list=ls())
setwd("~/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/BenchmarkR/COSMIC_test/subset/")
library(microbenchmark)

#-----------------
## Reading TSV
# base, readr, vroom
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
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
#---------------------------
# assigning vs concatenating

test.name <- "assignVconcat"
dat <- utils::read.table(file = "100001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)

source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(10,100,1000,10000,100000)){
  strings <- dat[1:i,"HGVSC"]
  bmark <- microbenchmark(
  "assign"={ # assign to a specific index  
    ENS_ID <- rep(NA,length(strings)) # Initialize an empty vector
    for(idx in seq_along(strings)){
      ENS_ID[idx] <- unlist(strsplit(x = strings[idx],split = ".",fixed =T),use.names = F)[1]
    }
  },
  "concat"={ # concat: concatenation
    ENS_ID <- c() # declare vector
    for(idx in seq_along(strings)){
      ENS_ID <- c(ENS_ID,unlist(strsplit(x = strings[idx],split = ".",fixed =T),use.names = F)[1])
    }
  }, "ideal"={ # ideal: vectorization (i.e. skip the for loop) – here it cycles
    ENS_ID <- unlist(strsplit(x = strings,split = ".",fixed =T),use.names = F)[seq(1,3*length(strings),by = 3)]
  }, times = 5)
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}
DF$size <- as.integer(format(DF$size,scientific = F))
saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))


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
  return(as.integer(base::substring(text = genomePos,first = 1,last = minus-1))-as.integer(base::substring(text = genomePos,first = minus+1)))
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
all <- utils::read.table(file = "1000001_d.tsv",header = T,sep = "\t",as.is = T,stringsAsFactors = T)
uniqueIDs <- unique(all$MUTATION_ID)

DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]

for(i in c(100,1000,10000,100000)){
  dat <- all[all$MUTATION_ID %in% uniqueIDs[1:i],]
  bmark <- microbenchmark(
    "lapply" ={
      res_l <- base::lapply(X = unique(dat$Gene.name), FUN = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T))
    },
    "parLapply" ={
      myCluster <- makeCluster(4, type = "FORK",useXDR=F,.combine=cbind)
      print(myCluster)
      registerDoParallel(myCluster)
      res_par <- snow::parLapply(cl = myCluster,X = unique(dat$Gene.name), fun = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T))
      stopCluster(myCluster)
    }, 
    "mclapply" = {
      res_mcl <- parallel::mclapply(X = unique(dat$Gene.name), FUN = function(X) mean(dat[dat$Gene.name==X,"FATHMM.score"],na.rm = T),mc.cores = parallel::detectCores())
    }, times = 5)
  # print(identical(res_l,res_par))
  # print(identical(res_mcl,res_par))
  saveRDS(bmark,file = paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  rm(results,bmark)
}

# ---- Re-Creating "results" of bmarks as it was not saved earlier.
rm(list=ls())
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
test.name <- "lapplyVmclapplyVparLapply"
DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,]
for(i in c(100,1000,10000,100000)){
  bmark <- readRDS(paste0("../bmark/bmark_",test.name,"_",i,".RDS"))
  results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
  results$size <- rep(i,length(results[,1]))
  DF <- rbind.data.frame(DF,results)
  # bmark_lapplyVmclapplyVparLapply_10.RDS
}

saveRDS(DF,file = paste0("../results/results_",test.name,".RDS"))
rm(list=ls())
gc()


# ---------- parallel saveRDS.gz()
source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
test.name <- "parallel_saveRDS"
file.prefix <- c("11", "101", "1001", "10001", "100001", "1000001")
DF <- data.frame(expr="",N=NA,time=NA,sd=NA,se=NA,ci=NA,size=NA,stringsAsFactors = F)
DF <- DF[-1,] 
source('https://gist.githubusercontent.com/dchakro/8b1e97ba6853563dd0bb5b7be2317692/raw/parallelRDS.R')
for(f in file.prefix){
  dat <- utils::read.table(file = paste0(f,"_d.tsv"),header = T,sep = "\t",as.is = T,stringsAsFactors = T)
  bmark <- microbenchmark(
    "base" = {
      base::saveRDS(object = dat, compress = "gzip",file = "/dev/null")
    },
    "parallel" = {
      saveRDS.gz(object = dat, threads = parallel::detectCores(), compression_level = 6, file = "/dev/null")
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



#----------  Removing columns from a table
source('https://gist.githubusercontent.com/dchakro/8b1e97ba6853563dd0bb5b7be2317692/raw/parallelRDS.R')
rm(list=ls()[!ls() %in% c("readRDS.gz","loadRDS","cmdAvail","saveRDS.gz","writeRDS")])
muts <- readRDS.gz(file = "/Users/deepankar/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/COSMIC/v92/Full_Database/4.COSMIC.all.coding.Mutatations.RDS")


library(microbenchmark)

testDF <- muts [1:200000,]
testDT <- data.table::as.data.table(testDF)
rm(muts)

removeCols <- c("ID_tumour","Accession.Number","Histology.subtype.1","Mutation.CDS","Mutation.genome.position","Mutation.somatic.status")

# ---> Automatic evaluation of commands
# expressionWithin <- parse(text=paste0("rm(",paste0(removeCols,collapse=","),")"))
# expressionSubset <- parse(text=paste0("c(",paste0(removeCols,collapse=","),")"))
# 
# rm(bmark)
# bmark <- microbenchmark(
#   DT_null = {tmp <- testDT[,as.vector(removeCols) := NULL]; rm(tmp)},
#   WITHIN = {tmp <- within(testDF,eval(expressionWithin)); rm(tmp)},
#   SUBSET = {tmp <- subset(testDF, select = -eval(expressionSubset)); rm(tmp)},
#   in_operator = {tmp <- testDF[,!names(testDF) %in% removeCols]; rm(tmp)},
#   which_op = {tmp <- testDF[,-which(colnames(testDF) %in% removeCols)]; rm(tmp)},
#   times=10)
# 
# source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
# rm(results)
# results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
# library(ggplot2)
# source('https://raw.githubusercontent.com/dchakro/ggplot_themes/master/DC_theme_generator.R')
# options(stringsAsFactors = F)
# 
# results$time <- results$time/1e+06
# results$sd <- results$sd/1e+06
# customtheme <- DC_theme_generator(type='L',x.axis.angle = 45)
# ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Populating a vector")+geom_text(position=position_dodge(width=0.9))

# ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/assignVconcat_bar.svg",height = 5,width = 14)

rm(bmark)
bmark <- microbenchmark(
  DT_null = {tmp <- testDT[,c("ID_tumour","Accession.Number","Histology.subtype.1","Mutation.CDS","Mutation.genome.position","Mutation.somatic.status") := NULL]; rm(tmp)},
  WITHIN = {tmp <- within(testDF,rm(ID_tumour,Accession.Number,Histology.subtype.1,Mutation.CDS,Mutation.genome.position,Mutation.somatic.status)); rm(tmp)},
  SUBSET = {tmp <- subset(testDF, select = -c(ID_tumour,Accession.Number,Histology.subtype.1,Mutation.CDS,Mutation.genome.position,Mutation.somatic.status)); rm(tmp)},
  in_operator = {tmp <- testDF[,!colnames(testDF) %in% removeCols]; rm(tmp)},
  which_op = {tmp <- testDF[,-which(colnames(testDF) %in% removeCols)]; rm(tmp)},
  times=10)

rm(results)
results <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
results$time <- results$time/1e+06
results$sd <- results$sd/1e+06
ggplot(data = results, aes(x = expr, y = time, label = round(x = time,digits = 2), fill = expr))+geom_col(width=0.5,position=position_dodge(width=0.9))+geom_errorbar(position=position_dodge(width=0.9),aes(ymin=time-sd,ymax=time+sd),linetype="solid",size=0.75,width=0.2)+customtheme+ylab("Time (µs)")+xlab("Method")+ggtitle("Removing columns by name")+geom_text(position=position_dodge(width=0.9))
ggsave("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/public/benchmarkR/results/removingColumns.svg",height = 5,width = 4)
