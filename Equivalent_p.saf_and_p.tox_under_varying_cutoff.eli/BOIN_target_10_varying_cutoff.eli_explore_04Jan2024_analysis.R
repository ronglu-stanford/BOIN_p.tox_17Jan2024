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
## -----------------------:
## BOIN_targetDLT.10_explore_04Jan2024_4352_varying_cutoff.R
## BOIN_targetDLT.10_explore_04Jan2024_811_varying_cutoff.R



## -----------------------target.10.cutoff.eli.70 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_cutoff.eli_0.7_total.n.30.rslt_2024_01_04_14_21_43_PST_4352.RData")
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
tmp$cutoff.eli <- 70/100
all.sim.rslt <- tmp

load("target.10_cutoff.eli_0.7_total.n.30.rslt_2024_01_04_14_22_26_PST_811.RData")
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
tmp$cutoff.eli <- 70/100
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 2

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb

save(boundary.rslt.tb, file="cutoff.eli70_target.10_all_possible_boundary_tables_04Jan2024.RData")

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
# [1] 3
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 2
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$cutoff.eli <- 70/100

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 2  10
save(target.10.summary, file="cutoff.eli70_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "cutoff.eli70_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n2 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, cutoff.eli = 70%)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.cutoff.eli.70.total.n.30.rslt_sim_20002_04Jan2024.pdf" (6 x 6)
p.tox.ticks
# [1] 0.11 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------

## -----------------------target.10.cutoff.eli.80 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_cutoff.eli_0.8_total.n.30.rslt_2024_01_04_14_30_34_PST_4352.RData")
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
tmp$cutoff.eli <- 80/100
all.sim.rslt <- tmp

load("target.10_cutoff.eli_0.8_total.n.30.rslt_2024_01_04_14_31_16_PST_811.RData")
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
tmp$cutoff.eli <- 80/100
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 4

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb

save(boundary.rslt.tb, file="cutoff.eli80_target.10_all_possible_boundary_tables_04Jan2024.RData")

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
# [1] 3
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 2
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$cutoff.eli <- 80/100

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 4  10
save(target.10.summary, file="cutoff.eli80_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "cutoff.eli80_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n4 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, cutoff.eli = 80%)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.cutoff.eli.80.total.n.30.rslt_sim_20002_04Jan2024.pdf" (6 x 6)
p.tox.ticks
# [1] 0.11 0.12 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------

## -----------------------target.10.cutoff.eli.90 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_cutoff.eli_0.9_total.n.30.rslt_2024_01_04_14_39_23_PST_4352.RData")
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
tmp$cutoff.eli <- 90/100
all.sim.rslt <- tmp

load("target.10_cutoff.eli_0.9_total.n.30.rslt_2024_01_04_14_40_03_PST_811.RData")
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
tmp$cutoff.eli <- 90/100
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 6

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb

save(boundary.rslt.tb, file="cutoff.eli90_target.10_all_possible_boundary_tables_04Jan2024.RData")

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
# [1] 3
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 4
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.25 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$cutoff.eli <- 90/100

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 6  10
save(target.10.summary, file="cutoff.eli90_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "cutoff.eli90_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n6 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, cutoff.eli = 90%)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.cutoff.eli.90.total.n.30.rslt_sim_20002_04Jan2024.pdf" (6 x 6)
p.tox.ticks
# [1] 0.11 0.12 0.25 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------

## -----------------------target.10.cutoff.eli.97 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_cutoff.eli_0.97_total.n.30.rslt_2024_01_04_14_48_18_PST_4352.RData")
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
tmp$cutoff.eli <- 97/100
all.sim.rslt <- tmp

load("target.10_cutoff.eli_0.97_total.n.30.rslt_2024_01_04_14_48_55_PST_811.RData")
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
tmp$cutoff.eli <- 97/100
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 12

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb

save(boundary.rslt.tb, file="cutoff.eli97_target.10_all_possible_boundary_tables_04Jan2024.RData")

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
# [1] 3
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.25 0.39 0.46 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$cutoff.eli <- 97/100

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 12  10
save(target.10.summary, file="cutoff.eli97_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "cutoff.eli97_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
# my.color.n12 <- unique(my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))+4])
# save(my.color.n12, file="my.color.n12_04Jan2024.RData")

plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n12[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n12 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, cutoff.eli = 97%)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.cutoff.eli.97.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.25 0.39 0.46 0.65 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------

## -----------------------target.10.cutoff.eli.99 ---------------------------
rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_04Jan2024")
load("target.10_cutoff.eli_0.99_total.n.30.rslt_2024_01_04_14_57_02_PST_4352.RData")
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
tmp$cutoff.eli <- 99/100
all.sim.rslt <- tmp

load("target.10_cutoff.eli_0.99_total.n.30.rslt_2024_01_04_14_57_37_PST_811.RData")
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
tmp$cutoff.eli <- 99/100
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 12

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb

save(boundary.rslt.tb, file="cutoff.eli99_target.10_all_possible_boundary_tables_04Jan2024.RData")

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
# [1] 3
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.000 0.068 0.090

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.11 0.12 0.25 0.39 0.46 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$cutoff.eli <- 99/100

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 12  10
save(target.10.summary, file="cutoff.eli99_target.10_all_tables_summary_04Jan2024.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "cutoff.eli99_target.10_all_tables_summary_04Jan2024.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
load("my.color.n36_29Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
# my.color.n12 <- unique(my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))+4])
# save(my.color.n12, file="my.color.n12_04Jan2024.RData")
load("my.color.n12_04Jan2024.RData")

plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n12[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n12 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, cutoff.eli = 99%)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.cutoff.eli.99.total.n.30.rslt_sim_20002_04Jan2024.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.25 0.39 0.46 0.65 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------

