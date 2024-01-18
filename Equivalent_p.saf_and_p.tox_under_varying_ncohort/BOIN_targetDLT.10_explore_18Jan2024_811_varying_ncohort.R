rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_18Jan2024")
#install.packages("BOIN")
library(BOIN)

set.seed(811)


get.boundary.with.tryCatch <- function(test.ncohort, my.p.saf, my.p.tox){
  tryCatch(
    expr = {
      return(get.boundary(target=0.1, ncohort=test.ncohort, cohortsize=3, n.earlystop = 12,
                          p.saf = my.p.saf, p.tox = my.p.tox, cutoff.eli = 0.95,
                          extrasafe = FALSE, offset = 0.05))
      message("Successfully executed the get.boundary() call.")
    },
    error = function(e){
      return(NULL)
      print(e)}
  )## tryCatch
}

for(test.ncohort in c(5:9,12,20,40,100)){
  target <- 0.1
  test.boundary <- NULL
  while(length(test.boundary)==0){
    my.p.saf <- runif(1,min=0,max=target-0.0000001)
    my.p.tox <- runif(1,min=target+0.0000001,max=1)
    test.boundary <- get.boundary.with.tryCatch(test.ncohort, my.p.saf, my.p.tox)
  }## while
  
  test.boundary$boundary_tab
  # Number of patients treated 3 6 9 12
  # Escalate if # of DLT <=    0 1 1  2
  # Deescalate if # of DLT >=  2 3 4  5
  # Eliminate if # of DLT >=   3 4 5  6
  as.vector(test.boundary$boundary_tab)
  # [1]  3  0  2  3  6  1  3  4  9  1  4  5 12  2  5  6
  boundary.rslt <- paste(t(as.vector(test.boundary$boundary_tab)), collapse=", ")
  
  tmp.rslt <- data.frame(my.p.saf, my.p.tox, boundary.rslt)
  tmp.rslt
  #    my.p.saf my.p.tox X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16
  # 1 0.1048837 0.470715  3  0  2  3  6  1  3  4  9   1   4   5  12   2   5   6
  
  saved.rslt <- tmp.rslt
  
  for(i in 1:100000){
    
    test.boundary <- NULL
    while(length(test.boundary)==0){
      my.p.saf <- runif(1,min=0,max=target-0.0000001)
      my.p.tox <- runif(1,min=target+0.0000001,max=1)
      test.boundary <- get.boundary.with.tryCatch(test.ncohort, my.p.saf, my.p.tox)
    }## while
    
    boundary.rslt <- paste(t(as.vector(test.boundary$boundary_tab)), collapse=", ")
    tmp.rslt <- data.frame(my.p.saf, my.p.tox, boundary.rslt)
    
    saved.rslt <- rbind(saved.rslt, tmp.rslt)
  }
  head(saved.rslt)
  
  file.name <- paste(paste("target.10_ncohort", test.ncohort,"rslt",
                           paste(gsub("[^[:alnum:]]", "_", Sys.time()),"PST",sep="_"), sep="_"),
                     "_811.RData", sep="")
  
  save(saved.rslt, file=file.name)
}

# get.boundary(0.4, ncohort=10, cohortsize=3, n.earlystop = 12,
#              +              p.saf = 0.361, p.tox = 0.439, cutoff.eli = 0.95,
#              +              extrasafe = FALSE, offset = 0.05)
# Error in get.boundary(0.4, ncohort = 10, cohortsize = 3, n.earlystop = 12,  : 
#                         the probability deemed safe cannot be higher than or too close to the target!
