rm(list=ls())
## ---------- reference:
## UBCRM_3plus3_simulation_test_26Dec2023.R
## simulation_3plus3_25Dec2023.R
## ----------------------:
## BOIN_earlystop_n12_explore_28Dec2023_420.R
## BOIN_earlystop_n12_explore_28Dec2023_912.R
## BOIN_earlystop_n12_explore_28Dec2023_66.R
# target = 0.25
# ncohort = 10
# cohortsize = 3
# my.p.saf <- runif(1,min=0,max=0.2)
# my.p.tox <- runif(1,min=0.3,max=0.5)
## ----------------------:
# target = 0.25
# ncohort = 10
# cohortsize = 3
# my.p.saf <- runif(1,min=0,max=0.249)
# my.p.tox <- runif(1,min=0.251,max=1)
# Error in get.boundary(target, ncohort, cohortsize, n.earlystop = 12, p.saf = my.p.saf,  : 
#                         the probability deemed safe cannot be higher than or too close to the target!
## ----------------------:
## BOIN_earlystop_n12_explore_28Dec2023_187.R
## BOIN_earlystop_n12_explore_28Dec2023_4352.R
## BOIN_earlystop_n12_explore_28Dec2023_811.R
# target = 0.25
# ncohort = 10
# cohortsize = 3
# my.p.saf <- runif(1,min=0,max=0.21)
# my.p.tox <- runif(1,min=0.29,max=1)
## -----------------------:
## BOIN_earlystop_n12_explore_04Jan2024_4352_varying_target.R
## BOIN_earlystop_n12_explore_04Jan2024_811_varying_target.R
## -----------------------:
## BOIN_earlystop_n18_explore_04Jan2024_4352_varying_target.R
## BOIN_earlystop_n18_explore_04Jan2024_811_varying_target.R
## -----------------------:
## BOIN_targetDLT.10_explore_02Jan2024_811_varying_n.earlystop.R
## BOIN_targetDLT.10_explore_02Jan2024_4352_varying_n.earlystop.R

## -----------------------target.10.earlystop.15 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_earlystop.n_15_total.n.30.rslt_2024_01_04_05_34_09_PST_4352.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.10
tmp$n.earlystop <- 15
all.sim.rslt <- tmp

load("target.10_earlystop.n_15_total.n.30.rslt_2024_01_04_05_35_23_PST_811.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.10
tmp$n.earlystop <- 15
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 21

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 60827 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 43511 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 2, 3, 12, 0, 3, 3 
# 23517 
# 3, 0, 1, 2, 6, 0, 1, 2, 9, 0, 2, 3, 12, 0, 2, 3 
# 21960 
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 1, 3, 3 
# 19232 
save(boundary.rslt.tb, file="earlystop15_target.10_all_possible_boundary_tables_04Jan2024.RData")

all.sim.rslt.target.10 <- all.sim.rslt
## ---------------:
#load("target.10_all_30_possible_boundary_tables_04Jan2024.RData")
target.10.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.10.summary$p.saf.min <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.10.summary$p.saf.min

target.10.summary$p.saf.max <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.10.summary$p.saf.max

target.10.summary$p.tox.min <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.10.summary$p.tox.min

target.10.summary$p.tox.max <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.10.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 4
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.041 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 8
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.17 0.25 0.33 0.39 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$n.earlystop <- 15

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 21  10
save(target.10.summary, file="earlystop15_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "earlystop15_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
# my.color.n10 <- my.color.n36[c(1:4,7:10, 15,18,29,30, 36)]
# save(my.color.n10, file="my.color.n10_04Jan2024.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n21 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, n.earlystop = 15)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.earlystop.15.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.17 0.25 0.33 0.39 0.65 1.00
p.saf.ticks
# [1] 0.000 0.041 0.068 0.090
## --------

## -----------------------target.10.earlystop.18 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_earlystop.n_18_total.n.30.rslt_2024_01_04_07_30_34_PST_4352.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.10
tmp$n.earlystop <- 18
all.sim.rslt <- tmp

load("target.10_earlystop.n_18_total.n.30.rslt_2024_01_04_07_49_34_PST_811.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.10
tmp$n.earlystop <- 18
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 28

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 60827 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 43511 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 2, 3, 12, 0, 3, 3 
# 23517 
# 3, 0, 1, 2, 6, 0, 1, 2, 9, 0, 2, 3, 12, 0, 2, 3 
# 21960 
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 1, 3, 3 
# 19232 
save(boundary.rslt.tb, file="earlystop18_target.10_all_possible_boundary_tables_04Jan2024.RData")

all.sim.rslt.target.10 <- all.sim.rslt
## ---------------:
#load("target.10_all_30_possible_boundary_tables_04Jan2024.RData")
target.10.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.10.summary$p.saf.min <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.10.summary$p.saf.min

target.10.summary$p.saf.max <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.10.summary$p.saf.max

target.10.summary$p.tox.min <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.10.summary$p.tox.min

target.10.summary$p.tox.max <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.10.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 5
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.026 0.041 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 8
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.17 0.25 0.33 0.39 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$n.earlystop <- 18

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 28  10
save(target.10.summary, file="earlystop18_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "earlystop18_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
# my.color.n10 <- my.color.n36[c(1:4,7:10, 15,18,29,30, 36)]
# save(my.color.n10, file="my.color.n10_04Jan2024.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n28 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, n.earlystop = 18)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.earlystop.18.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.17 0.25 0.33 0.39 0.65 1.00
p.saf.ticks
# [1] 0.000 0.026 0.041 0.068 0.090
## --------

## -----------------------target.10.earlystop.21 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_earlystop.n_21_total.n.30.rslt_2024_01_04_08_00_56_PST_4352.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.10
tmp$n.earlystop <- 21
all.sim.rslt <- tmp

load("target.10_earlystop.n_21_total.n.30.rslt_2024_01_04_08_02_33_PST_811.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.10
tmp$n.earlystop <- 21
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 28

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 60827 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 43511 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 2, 3, 12, 0, 3, 3 
# 23517 
# 3, 0, 1, 2, 6, 0, 1, 2, 9, 0, 2, 3, 12, 0, 2, 3 
# 21960 
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 1, 3, 3 
# 19232 
save(boundary.rslt.tb, file="earlystop21_target.10_all_possible_boundary_tables_04Jan2024.RData")

all.sim.rslt.target.10 <- all.sim.rslt
## ---------------:
#load("target.10_all_30_possible_boundary_tables_04Jan2024.RData")
target.10.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.10.summary$p.saf.min <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.10.summary$p.saf.min

target.10.summary$p.saf.max <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.10.summary$p.saf.max

target.10.summary$p.tox.min <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.10.summary$p.tox.min

target.10.summary$p.tox.max <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.10.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 6
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.017 0.026 0.041 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 10
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.17 0.19 0.25 0.31 0.33 0.39 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$n.earlystop <- 21

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 45  10
save(target.10.summary, file="earlystop21_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "earlystop21_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
my.color.n60 <- c(my.color.n36, colors()[584:606])
my.color.n60.v2 <- sample(my.color.n60,60,replace = F)
my.color.n60.v2[31] <- "yellow"
my.color.n60 <- my.color.n60.v2
save(my.color.n60, file="my.color.n60_04Jan2024.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n60[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n45 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, n.earlystop = 21)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.earlystop.21.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.17 0.19 0.25 0.31 0.33 0.39 0.65 1.00
p.saf.ticks
# [1] 0.000 0.026 0.041 0.068 0.090
## --------

## -----------------------target.10.earlystop.24 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_earlystop.n_24_total.n.30.rslt_2024_01_04_08_13_52_PST_4352.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.10
tmp$n.earlystop <- 24
all.sim.rslt <- tmp

load("target.10_earlystop.n_24_total.n.30.rslt_2024_01_04_08_15_21_PST_811.RData")
ls()
saved.target.10.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.10.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.10.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.10
tmp$n.earlystop <- 24
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 60

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 60827 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 0, 3, 3 
# 43511 
# 3, 0, 1, 2, 6, 0, 2, 2, 9, 0, 2, 3, 12, 0, 3, 3 
# 23517 
# 3, 0, 1, 2, 6, 0, 1, 2, 9, 0, 2, 3, 12, 0, 2, 3 
# 21960 
# 3, 0, 2, 2, 6, 0, 2, 2, 9, 0, 3, 3, 12, 1, 3, 3 
# 19232 
save(boundary.rslt.tb, file="earlystop24_target.10_all_possible_boundary_tables_04Jan2024.RData")

all.sim.rslt.target.10 <- all.sim.rslt
## ---------------:
#load("target.10_all_30_possible_boundary_tables_04Jan2024.RData")
target.10.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.10.summary$p.saf.min <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.10.summary$p.saf.min

target.10.summary$p.saf.max <- sapply(target.10.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.10$my.p.saf[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.10.summary$p.saf.max

target.10.summary$p.tox.min <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.10.summary$p.tox.min

target.10.summary$p.tox.max <- sapply(target.10.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.10$my.p.tox[which(all.sim.rslt.target.10$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.10.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 7
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.012 0.017 0.026 0.041 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 11
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.15 0.17 0.19 0.25 0.31 0.33 0.39 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$n.earlystop <- 24

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 60  10
save(target.10.summary, file="earlystop24_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "earlystop24_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n60_04Jan2024.RData")
# my.color.n60 <- c(my.color.n36, colors()[584:606])
my.color.n60.v2 <- sample(my.color.n60,60,replace = F)
my.color.n60 <- my.color.n60.v2
save(my.color.n60, file="my.color.n60_v2_04Jan2024.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n60[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n60 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, n.earlystop = 24)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.earlystop.24.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.17 0.19 0.25 0.31 0.33 0.39 0.65 1.00
p.saf.ticks
# [1] 0.000 0.026 0.041 0.068 0.090
## --------

