rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_09Jan2024")
#install.packages("BOIN")
library(BOIN)

set.seed(369)

boundary.3plus3 <- paste(t(c(3, 0, 2, 2, 6, 1, 2, 2)), collapse=", ")
ncohort = 10
#cohortsize = 3
#target = 0.25

get.boundary.with.tryCatch <- function(target, my.p.saf, my.p.tox, test.cutoff, test.offset){
  tryCatch(
    expr = {
      return(get.boundary(target, ncohort=10, cohortsize=3, n.earlystop = 6,
                          p.saf = my.p.saf, p.tox = my.p.tox, cutoff.eli = test.cutoff,
                          extrasafe = T, offset = test.offset))
      message("Successfully executed the get.boundary() call.")
    },
    error = function(e){
      return(NULL)
      print(e)}
  )## tryCatch
}

for(test.offset in c(0.01, 0.05,0.1,0.2,0.3,0.4,0.49)){
  
  test.boundary <- NULL
  while(length(test.boundary)==0){
    target <- runif(1,min=0,max=0.5)
    my.p.saf <- runif(1,min=0,max=target-0.0000001)
    my.p.tox <- runif(1,min=target+0.0000001,max=1)
    test.cutoff <- runif(1,min=0,max=1)
    test.boundary <- get.boundary.with.tryCatch(target, my.p.saf, my.p.tox, test.cutoff, test.offset)
  }## while
  
  test.boundary$boundary_tab
  # Number of patients treated 3 6
  # Escalate if # of DLT <=    0 0
  # Deescalate if # of DLT >=  1 1
  # Eliminate if # of DLT >=   1 1
  as.vector(test.boundary$boundary_tab)
  # [1] 3 0 1 1 6 0 1 1
  
  boundary.rslt <- paste(t(as.vector(test.boundary$boundary_tab)), collapse=", ")
  
  tmp.rslt <- data.frame(target, my.p.saf, my.p.tox, test.cutoff, test.offset, boundary.rslt)
  tmp.rslt
  
  saved.rslt <- tmp.rslt
  
  for(i in 1:100000){
    
    test.boundary <- NULL
    while(length(test.boundary)==0){
      target <- runif(1,min=0,max=0.5)
      my.p.saf <- runif(1,min=0,max=target-0.0000001)
      my.p.tox <- runif(1,min=target+0.0000001,max=1)
      test.cutoff <- runif(1,min=0,max=1)
      test.boundary <- get.boundary.with.tryCatch(target, my.p.saf, my.p.tox, test.cutoff, test.offset)
    }## while
    
    boundary.rslt <- paste(t(as.vector(test.boundary$boundary_tab)), collapse=", ")
    if(boundary.rslt==boundary.3plus3){
      tmp.rslt <- data.frame(target, my.p.saf, my.p.tox, test.cutoff, test.offset, boundary.rslt)
      saved.rslt <- rbind(saved.rslt, tmp.rslt)
    }
  }
  head(saved.rslt)
  
  file.name <- paste(paste("BOIN_3plus3_explore_09Jan2024",
                           paste(gsub("[^[:alnum:]]", "_", Sys.time()),"PST",sep="_"), sep="_"),
                     "_369.RData", sep="")
  
  save(saved.rslt, file=file.name)
}
dim(saved.rslt)
# get.boundary(0.4, ncohort=10, cohortsize=3, n.earlystop = 12,
#              +              p.saf = 0.361, p.tox = 0.439, cutoff.eli = 0.95,
#              +              extrasafe = FALSE, offset = 0.05)
# Error in get.boundary(0.4, ncohort = 10, cohortsize = 3, n.earlystop = 12,  : 
#                         the probability deemed safe cannot be higher than or too close to the target!
