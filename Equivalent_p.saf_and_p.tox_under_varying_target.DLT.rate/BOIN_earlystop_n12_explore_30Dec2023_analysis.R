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
## BOIN_earlystop_n12_explore_30Dec2023_4352_varying_target.R
## BOIN_earlystop_n12_explore_30Dec2023_811_varying_target.R
## -----------------------:
## BOIN_earlystop_n18_explore_30Dec2023_4352_varying_target.R
## BOIN_earlystop_n18_explore_30Dec2023_811_varying_target.R


## -----------------------target.10.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_10_total.n.30.rslt_2023_12_30_16_59_34_PST_4352.RData")
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
all.sim.rslt <- tmp

load("earlystop12_target_10_total.n.30.rslt_2023_12_30_16_58_42_PST_811.RData")
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
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 10

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
save(boundary.rslt.tb, file="earlystop12_target.10_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.10 <- all.sim.rslt
## ---------------:
#load("target.10_all_30_possible_boundary_tables_29Dec2023.RData")
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
# [1] 2
sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
# [1] 0.00 0.06

length(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 5
sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))
# [1] 0.14 0.25 0.39 0.65 1.00
target.10.summary$sim.freq <- boundary.rslt.tb
target.10.summary$target <- 0.10
target.10.summary$ncohort <- 10
target.10.summary$cohortsize <- 3
target.10.summary$n.earlystop <- 12

sum(target.10.summary$sim.freq)
# [1] 200002
dim(target.10.summary)
# [1] 4  10
save(target.10.summary, file="earlystop12_target.10_all_tables_summary_30Dec2023.RData")

target.10.summary <- target.10.summary[order(round(target.10.summary$p.saf.min, 3), round(target.10.summary$p.tox.min,2)),]
write.csv(target.10.summary, file = "earlystop12_target.10_all_tables_summary_29Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.10.summary$p.saf.min, target.10.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.10.summary$p.tox.min, target.10.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
my.color.n10 <- my.color.n36[c(1:4,7:10, 15,18,29,30, 36)]
save(my.color.n10, file="my.color.n10_30Dec2023.RData")
#all.sim.rslt <- all.sim.rslt.target.10[1:50000,]
all.sim.rslt <- all.sim.rslt.target.10
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n10[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n10 BOIN boundary tables indicated by different colors\n (target DLT rate = 10%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.10.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.11 0.12 0.25 0.39 0.65 1.00
p.saf.ticks
# [1] 0.000 0.068 0.090
## --------


## -----------------------target.15.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_15_total.n.30.rslt_2023_12_30_17_07_00_PST_4352.RData")
ls()
saved.target.15.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.15.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.15.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.15
all.sim.rslt <- tmp

load("earlystop12_target_15_total.n.30.rslt_2023_12_30_17_05_57_PST_811.RData")
ls()
saved.target.15.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.15.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.15.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.15
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 15

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 2, 6, 0, 3, 3, 9, 1, 3, 3, 12, 1, 4, 4 
# 43676 
# 3, 0, 2, 2, 6, 0, 3, 3, 9, 0, 3, 3, 12, 1, 4, 4 
# 31535 
# 3, 0, 2, 2, 6, 0, 3, 3, 9, 0, 3, 3, 12, 0, 4, 4 
# 30337 
# 3, 0, 1, 2, 6, 0, 2, 3, 9, 1, 3, 3, 12, 1, 4, 4 
# 18685 
# 3, 0, 1, 2, 6, 0, 2, 3, 9, 0, 3, 3, 12, 1, 4, 4 
# 1319
save(boundary.rslt.tb, file="earlystop12_target.15_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.15 <- all.sim.rslt
## ---------------:
#load("target.15_all_30_possible_boundary_tables_29Dec2023.RData")
target.15.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.15.summary$p.saf.min <- sapply(target.15.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.15$my.p.saf[which(all.sim.rslt.target.15$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.15.summary$p.saf.min

target.15.summary$p.saf.max <- sapply(target.15.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.15$my.p.saf[which(all.sim.rslt.target.15$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.15.summary$p.saf.max

target.15.summary$p.tox.min <- sapply(target.15.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.15$my.p.tox[which(all.sim.rslt.target.15$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.15.summary$p.tox.min

target.15.summary$p.tox.max <- sapply(target.15.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.15$my.p.tox[which(all.sim.rslt.target.15$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.15.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.15.summary$p.saf.min, target.15.summary$p.saf.max), 3)))
# [1] 4
sort(unique(round(c(target.15.summary$p.saf.min, target.15.summary$p.saf.max), 3)))
# [1] 0.000 0.039 0.079 0.135

length(unique(round(c(target.15.summary$p.tox.min, target.15.summary$p.tox.max),2)))
# [1] 6
sort(unique(round(c(target.15.summary$p.tox.min, target.15.summary$p.tox.max),2)))
# [1] 0.17 0.18 0.31 0.37 0.56 1.00
target.15.summary$sim.freq <- boundary.rslt.tb
target.15.summary$target <- 0.15
target.15.summary$ncohort <- 10
target.15.summary$cohortsize <- 3
target.15.summary$n.earlystop <- 12

sum(target.15.summary$sim.freq)
# [1] 200002
dim(target.15.summary)
# [1] 15  10
save(target.15.summary, file="earlystop12_target.15_all_tables_summary_30Dec2023.RData")

target.15.summary <- target.15.summary[order(round(target.15.summary$p.saf.min, 3), round(target.15.summary$p.tox.min,2)),]
write.csv(target.15.summary, file = "earlystop12_target.15_all_tables_summary_29Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.15.summary$p.saf.min, target.15.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.15.summary$p.tox.min, target.15.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
my.color.n15 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29)]
save(my.color.n15, file="my.color.n15_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.15[1:50000,]
all.sim.rslt <- all.sim.rslt.target.15
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n15[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n15 BOIN boundary tables indicated by different colors\n (target DLT rate = 15%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.15.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.17 0.18 0.31 0.37 0.56 1.00
p.saf.ticks
# [1] 0.000 0.039 0.079 0.135
## --------


## -----------------------target.20.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_20_total.n.30.rslt_2023_12_30_17_14_29_PST_4352.RData")
ls()
saved.target.20.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.20.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.20.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.20
all.sim.rslt <- tmp

load("earlystop12_target_20_total.n.30.rslt_2023_12_30_17_13_18_PST_811.RData")
ls()
saved.target.20.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.20.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.20.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.20
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 16

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.20_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.20 <- all.sim.rslt
## ---------------:
#load("target.20_all_30_possible_boundary_tables_29Dec2023.RData")
target.20.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.20.summary$p.saf.min <- sapply(target.20.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.20$my.p.saf[which(all.sim.rslt.target.20$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.20.summary$p.saf.min

target.20.summary$p.saf.max <- sapply(target.20.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.20$my.p.saf[which(all.sim.rslt.target.20$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.20.summary$p.saf.max

target.20.summary$p.tox.min <- sapply(target.20.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.20$my.p.tox[which(all.sim.rslt.target.20$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.20.summary$p.tox.min

target.20.summary$p.tox.max <- sapply(target.20.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.20$my.p.tox[which(all.sim.rslt.target.20$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.20.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.20.summary$p.saf.min, target.20.summary$p.saf.max), 3)))
# [1] 5
sort(unique(round(c(target.20.summary$p.saf.min, target.20.summary$p.saf.max), 3)))
# [1] 0.000 0.022 0.051 0.137 0.180

length(unique(round(c(target.20.summary$p.tox.min, target.20.summary$p.tox.max),2)))
# [1] 5
sort(unique(round(c(target.20.summary$p.tox.min, target.20.summary$p.tox.max),2)))
# [1] 0.22 0.25 0.30 0.49 1.00
target.20.summary$sim.freq <- boundary.rslt.tb
target.20.summary$target <- 0.20
target.20.summary$ncohort <- 10
target.20.summary$cohortsize <- 3
target.20.summary$n.earlystop <- 12

sum(target.20.summary$sim.freq)
# [1] 200002
dim(target.20.summary)
# [1] 16  10
save(target.20.summary, file="earlystop12_target.20_all_tables_summary_30Dec2023.RData")

target.20.summary <- target.20.summary[order(round(target.20.summary$p.saf.min, 3), round(target.20.summary$p.tox.min,2)),]
write.csv(target.20.summary, file = "earlystop12_target.20_all_tables_summary_29Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.20.summary$p.saf.min, target.20.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.20.summary$p.tox.min, target.20.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
save(my.color.n16, file="my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.20[1:50000,]
all.sim.rslt <- all.sim.rslt.target.20
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n16[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n16 BOIN boundary tables indicated by different colors\n (target DLT rate = 20%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.20.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.22 0.25 0.30 0.49 1.00
p.saf.ticks
# [1] 0.000 0.022 0.051 0.137 0.180
## --------


## -----------------------target.21.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_21_total.n.30.rslt_2023_12_30_17_22_04_PST_4352.RData")
ls()
saved.target.21.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.21.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.21.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.21
all.sim.rslt <- tmp

load("earlystop12_target_21_total.n.30.rslt_2023_12_30_17_20_47_PST_811.RData")
ls()
saved.target.21.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.21.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.21.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.21
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 16

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.21_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.21 <- all.sim.rslt
## ---------------:
#load("target.21_all_30_possible_boundary_tables_29Dec2023.RData")
target.21.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.21.summary$p.saf.min <- sapply(target.21.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.21$my.p.saf[which(all.sim.rslt.target.21$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.21.summary$p.saf.min

target.21.summary$p.saf.max <- sapply(target.21.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.21$my.p.saf[which(all.sim.rslt.target.21$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.21.summary$p.saf.max

target.21.summary$p.tox.min <- sapply(target.21.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.21$my.p.tox[which(all.sim.rslt.target.21$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.21.summary$p.tox.min

target.21.summary$p.tox.max <- sapply(target.21.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.21$my.p.tox[which(all.sim.rslt.target.21$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.21.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.21.summary$p.saf.min, target.21.summary$p.saf.max), 3)))
# [1] 8
sort(unique(round(c(target.21.summary$p.saf.min, target.21.summary$p.saf.max), 3)))
# [1] 0.000 0.019 0.020 0.046 0.047 0.128 0.129 0.189

length(unique(round(c(target.21.summary$p.tox.min, target.21.summary$p.tox.max),2)))
# [1] 4
sort(unique(round(c(target.21.summary$p.tox.min, target.21.summary$p.tox.max),2)))
# [1] 0.23 0.29 0.47 1.00
target.21.summary$sim.freq <- boundary.rslt.tb
target.21.summary$target <- 0.21
target.21.summary$ncohort <- 10
target.21.summary$cohortsize <- 3
target.21.summary$n.earlystop <- 12

sum(target.21.summary$sim.freq)
# [1] 200002
dim(target.21.summary)
# [1] 16  10
save(target.21.summary, file="earlystop12_target.21_all_tables_summary_30Dec2023.RData")

target.21.summary <- target.21.summary[order(round(target.21.summary$p.saf.min, 3), round(target.21.summary$p.tox.min,2)),]
write.csv(target.21.summary, file = "earlystop12_target.21_all_tables_summary_29Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.21.summary$p.saf.min, target.21.summary$p.saf.max), 2)))
p.tox.ticks <- sort(unique(round(c(target.21.summary$p.tox.min, target.21.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
## load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
load("my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.21[1:50000,]
all.sim.rslt <- all.sim.rslt.target.21
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n16[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n16 BOIN boundary tables indicated by different colors\n (target DLT rate = 21%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.21.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.23 0.29 0.47 1.00
p.saf.ticks
# [1] 0.00 0.02 0.05 0.13 0.19
## --------


## -----------------------target.22.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_22_total.n.30.rslt_2023_12_30_17_29_41_PST_4352.RData")
ls()
saved.target.22.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.22.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.22.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.22
all.sim.rslt <- tmp

load("earlystop12_target_22_total.n.30.rslt_2023_12_30_17_28_13_PST_811.RData")
ls()
saved.target.22.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.22.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.22.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.22
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 12

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.22_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.22 <- all.sim.rslt
## ---------------:
#load("target.22_all_30_possible_boundary_tables_29Dec2023.RData")
target.22.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.22.summary$p.saf.min <- sapply(target.22.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.22$my.p.saf[which(all.sim.rslt.target.22$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.22.summary$p.saf.min

target.22.summary$p.saf.max <- sapply(target.22.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.22$my.p.saf[which(all.sim.rslt.target.22$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.22.summary$p.saf.max

target.22.summary$p.tox.min <- sapply(target.22.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.22$my.p.tox[which(all.sim.rslt.target.22$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.22.summary$p.tox.min

target.22.summary$p.tox.max <- sapply(target.22.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.22$my.p.tox[which(all.sim.rslt.target.22$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.22.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.22.summary$p.saf.min, target.22.summary$p.saf.max), 3)))
# [1] 5
sort(unique(round(c(target.22.summary$p.saf.min, target.22.summary$p.saf.max), 3)))
# [1] 0.000 0.017 0.043 0.121 0.198

length(unique(round(c(target.22.summary$p.tox.min, target.22.summary$p.tox.max),2)))
# [1] 4
sort(unique(round(c(target.22.summary$p.tox.min, target.22.summary$p.tox.max),2)))
# [1] 0.24 0.28 0.46 1.00
target.22.summary$sim.freq <- boundary.rslt.tb
target.22.summary$target <- 0.22
target.22.summary$ncohort <- 10
target.22.summary$cohortsize <- 3
target.22.summary$n.earlystop <- 12

sum(target.22.summary$sim.freq)
# [1] 200002
dim(target.22.summary)
# [1] 12  10
save(target.22.summary, file="earlystop12_target.22_all_tables_summary_30Dec2023.RData")

target.22.summary <- target.22.summary[order(round(target.22.summary$p.saf.min, 3), round(target.22.summary$p.tox.min,2)),]
write.csv(target.22.summary, file = "earlystop12_target.22_all_tables_summary_30Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.22.summary$p.saf.min, target.22.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.22.summary$p.tox.min, target.22.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
## load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
load("my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.22[1:50000,]
all.sim.rslt <- all.sim.rslt.target.22
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n16[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n12 BOIN boundary tables indicated by different colors\n (target DLT rate = 22%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.22.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.24 0.28 0.46 1.00
p.saf.ticks
# [1] 0.000 0.017 0.043 0.121 0.198
## --------


## -----------------------target.25.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_25_total.n.30.rslt_2023_12_30_17_52_46_PST_4352.RData")
ls()
saved.target.25.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.25
all.sim.rslt <- tmp

load("earlystop12_target_25_total.n.30.rslt_2023_12_30_17_50_45_PST_811.RData")
ls()
saved.target.25.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.25
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 30

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.25_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.25 <- all.sim.rslt
## ---------------:
#load("target.25_all_30_possible_boundary_tables_29Dec2023.RData")
target.25.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.25.summary$p.saf.min <- sapply(target.25.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.25$my.p.saf[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.25.summary$p.saf.min

target.25.summary$p.saf.max <- sapply(target.25.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.25$my.p.saf[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.25.summary$p.saf.max

target.25.summary$p.tox.min <- sapply(target.25.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.25$my.p.tox[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.25.summary$p.tox.min

target.25.summary$p.tox.max <- sapply(target.25.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.25$my.p.tox[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.25.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
# [1] 6
sort(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
# [1] 0.000 0.012 0.033 0.101 0.196 0.225

length(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))
# [1] 0.28 0.42 0.60 0.65 0.75 0.95 1.00
target.25.summary$sim.freq <- boundary.rslt.tb
target.25.summary$target <- 0.25
target.25.summary$ncohort <- 10
target.25.summary$cohortsize <- 3
target.25.summary$n.earlystop <- 12

sum(target.25.summary$sim.freq)
# [1] 200002
dim(target.25.summary)
# [1] 30  10
save(target.25.summary, file="earlystop12_target.25_all_tables_summary_30Dec2023.RData")

target.25.summary <- target.25.summary[order(round(target.25.summary$p.saf.min, 3), round(target.25.summary$p.tox.min,2)),]
write.csv(target.25.summary, file = "earlystop12_target.25_all_tables_summary_30Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
#load("my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.25[1:50000,]
all.sim.rslt <- all.sim.rslt.target.25
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n30 BOIN boundary tables indicated by different colors\n (target DLT rate = 25%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.25.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.28 0.42 0.60 0.65 0.75 0.95 1.00
p.saf.ticks
# [1] 0.000 0.012 0.033 0.101 0.196 0.225
## --------


## -----------------------target.30.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_30_total.n.30.rslt_2023_12_30_18_31_42_PST_4352.RData")
ls()
saved.target.30.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.30.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.30.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.30
all.sim.rslt <- tmp

load("earlystop12_target_30_total.n.30.rslt_2023_12_30_18_28_48_PST_811.RData")
ls()
saved.target.30.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.30.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.30.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.30
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 36

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.30_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.30 <- all.sim.rslt
## ---------------:
#load("target.30_all_30_possible_boundary_tables_29Dec2023.RData")
target.30.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.30.summary$p.saf.min <- sapply(target.30.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.30$my.p.saf[which(all.sim.rslt.target.30$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.30.summary$p.saf.min

target.30.summary$p.saf.max <- sapply(target.30.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.30$my.p.saf[which(all.sim.rslt.target.30$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.30.summary$p.saf.max

target.30.summary$p.tox.min <- sapply(target.30.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.30$my.p.tox[which(all.sim.rslt.target.30$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.30.summary$p.tox.min

target.30.summary$p.tox.max <- sapply(target.30.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.30$my.p.tox[which(all.sim.rslt.target.30$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.30.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.30.summary$p.saf.min, target.30.summary$p.saf.max), 3)))
# [1] 7
sort(unique(round(c(target.30.summary$p.saf.min, target.30.summary$p.saf.max), 3)))
# [1] 0.000 0.006 0.020 0.074 0.156 0.204 0.270

length(unique(round(c(target.30.summary$p.tox.min, target.30.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.30.summary$p.tox.min, target.30.summary$p.tox.max),2)))
# [1] 0.33 0.37 0.54 0.60 0.70 0.93 1.00
target.30.summary$sim.freq <- boundary.rslt.tb
target.30.summary$target <- 0.30
target.30.summary$ncohort <- 10
target.30.summary$cohortsize <- 3
target.30.summary$n.earlystop <- 12

sum(target.30.summary$sim.freq)
# [1] 200002
dim(target.30.summary)
# [1] 36  10
save(target.30.summary, file="earlystop12_target.30_all_tables_summary_30Dec2023.RData")

target.30.summary <- target.30.summary[order(round(target.30.summary$p.saf.min, 3), round(target.30.summary$p.tox.min,2)),]
write.csv(target.30.summary, file = "earlystop12_target.30_all_tables_summary_30Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.30.summary$p.saf.min, target.30.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.30.summary$p.tox.min, target.30.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
#load("my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.30[1:50000,]
all.sim.rslt <- all.sim.rslt.target.30
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n36 BOIN boundary tables indicated by different colors\n (target DLT rate = 30%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.30.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.33 0.37 0.54 0.60 0.70 0.93 1.00
p.saf.ticks
# [1] 0.000 0.006 0.020 0.074 0.156 0.204 0.270
## --------


## -----------------------target.35.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_35_total.n.30.rslt_2023_12_30_19_11_04_PST_4352.RData")
ls()
saved.target.35.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.35.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.35.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.35
all.sim.rslt <- tmp

load("earlystop12_target_35_total.n.30.rslt_2023_12_30_19_07_04_PST_811.RData")
ls()
saved.target.35.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.35.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.35.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.35
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 36

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.35_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.35 <- all.sim.rslt
## ---------------:
#load("target.35_all_30_possible_boundary_tables_29Dec2023.RData")
target.35.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.35.summary$p.saf.min <- sapply(target.35.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.35$my.p.saf[which(all.sim.rslt.target.35$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.35.summary$p.saf.min

target.35.summary$p.saf.max <- sapply(target.35.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.35$my.p.saf[which(all.sim.rslt.target.35$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.35.summary$p.saf.max

target.35.summary$p.tox.min <- sapply(target.35.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.35$my.p.tox[which(all.sim.rslt.target.35$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.35.summary$p.tox.min

target.35.summary$p.tox.max <- sapply(target.35.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.35$my.p.tox[which(all.sim.rslt.target.35$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.35.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.35.summary$p.saf.min, target.35.summary$p.saf.max), 3)))
# [1] 7
sort(unique(round(c(target.35.summary$p.saf.min, target.35.summary$p.saf.max), 3)))
# [1] 0.000 0.003 0.012 0.053 0.122 0.165 0.315

length(unique(round(c(target.35.summary$p.tox.min, target.35.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.35.summary$p.tox.min, target.35.summary$p.tox.max),2)))
# [1] 0.39 0.49 0.54 0.65 0.75 0.90 1.00
target.35.summary$sim.freq <- boundary.rslt.tb
target.35.summary$target <- 0.35
target.35.summary$ncohort <- 10
target.35.summary$cohortsize <- 3
target.35.summary$n.earlystop <- 12

sum(target.35.summary$sim.freq)
# [1] 200002
dim(target.35.summary)
# [1] 36  10
save(target.35.summary, file="earlystop12_target.35_all_tables_summary_30Dec2023.RData")

target.35.summary <- target.35.summary[order(round(target.35.summary$p.saf.min, 3), round(target.35.summary$p.tox.min,2)),]
write.csv(target.35.summary, file = "earlystop12_target.35_all_tables_summary_30Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.35.summary$p.saf.min, target.35.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.35.summary$p.tox.min, target.35.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
#load("my.color.n16_30Dec2023.RData")

#all.sim.rslt <- all.sim.rslt.target.35[1:50000,]
all.sim.rslt <- all.sim.rslt.target.35
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n36 BOIN boundary tables indicated by different colors\n (target DLT rate = 35%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.35.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.39 0.49 0.54 0.65 0.75 0.90 1.00
p.saf.ticks
# 0.000 0.003 0.012 0.053 0.122 0.165 0.315
## --------


## -----------------------target.40.earlystop.12 ---------------------------
rm(list=ls())
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_30Dec2023")
load("earlystop12_target_40_total.n.30.rslt_2023_12_30_19_19_06_PST_4352.RData")
ls()
saved.target.40.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.40.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.40.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.40
all.sim.rslt <- tmp

load("earlystop12_target_40_total.n.30.rslt_2023_12_30_19_14_56_PST_811.RData")
ls()
saved.target.40.total.n.30.rslt <- saved.rslt
rm(saved.rslt)
dim(saved.target.40.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.40.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.40
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 42

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="earlystop12_target.40_all_possible_boundary_tables_30Dec2023.RData")

all.sim.rslt.target.40 <- all.sim.rslt
## ---------------:
#load("target.40_all_30_possible_boundary_tables_29Dec2023.RData")
target.40.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.40.summary$p.saf.min <- sapply(target.40.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.40$my.p.saf[which(all.sim.rslt.target.40$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.40.summary$p.saf.min

target.40.summary$p.saf.max <- sapply(target.40.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.40$my.p.saf[which(all.sim.rslt.target.40$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.40.summary$p.saf.max

target.40.summary$p.tox.min <- sapply(target.40.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.40$my.p.tox[which(all.sim.rslt.target.40$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.40.summary$p.tox.min

target.40.summary$p.tox.max <- sapply(target.40.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.40$my.p.tox[which(all.sim.rslt.target.40$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.40.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.40.summary$p.saf.min, target.40.summary$p.saf.max), 3)))
# [1] 9
sort(unique(round(c(target.40.summary$p.saf.min, target.40.summary$p.saf.max), 3)))
# [1] 0.000 0.001 0.002 0.007 0.038 0.095 0.132 0.271 0.360

length(unique(round(c(target.40.summary$p.tox.min, target.40.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.40.summary$p.tox.min, target.40.summary$p.tox.max),2)))
# [1] 0.44 0.49 0.60 0.70 0.75 0.87 1.00
target.40.summary$sim.freq <- boundary.rslt.tb
target.40.summary$target <- 0.40
target.40.summary$ncohort <- 10
target.40.summary$cohortsize <- 3
target.40.summary$n.earlystop <- 12

sum(target.40.summary$sim.freq)
# [1] 200002
dim(target.40.summary)
# [1] 42  10
save(target.40.summary, file="earlystop12_target.40_all_tables_summary_30Dec2023.RData")

target.40.summary <- target.40.summary[order(round(target.40.summary$p.saf.min, 3), round(target.40.summary$p.tox.min,2)),]
write.csv(target.40.summary, file = "earlystop12_target.40_all_tables_summary_30Dec2023.csv", row.names = F, na = "")

## ------- : 
p.saf.ticks <- sort(unique(round(c(target.40.summary$p.saf.min, target.40.summary$p.saf.max), 3)))
p.tox.ticks <- sort(unique(round(c(target.40.summary$p.tox.min, target.40.summary$p.tox.max),2)))

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
## load("my_color_v3_28Dec2023.Rata")
# save(my.color.n36, file="my.color.n36_29Dec2023.RData")
load("my.color.n36_29Dec2023.RData")
## my.color.n16 <- my.color.n36[c(1,3:4,7:10, 15,18,29,30, 2,36, 34,21,13,25,29,5)]
# save(my.color.n16, file="my.color.n16_30Dec2023.RData")
#load("my.color.n16_30Dec2023.RData")
my.color.n42 <- c(my.color.n36,"violetred4","steelblue4","violetred1","yellowgreen","thistle4","turquoise4")
#all.sim.rslt <- all.sim.rslt.target.40[1:50000,]
all.sim.rslt <- all.sim.rslt.target.40
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n42[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n42 BOIN boundary tables indicated by different colors\n (target DLT rate = 40%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.40.earlystop.12.total.n.30.rslt_sim_20002_30Dec2023.pdf" (10 x 8)
p.tox.ticks
# [1] 0.44 0.49 0.60 0.70 0.75 0.87 1.00
p.saf.ticks
# [1] 0.000 0.001 0.002 0.007 0.038 0.095 0.132 0.271 0.360
## --------

## ------------------- work of 12/30/2023 ends by this line ----------------------











## -----------------------target.25.earlystop.12 ---------------------------
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_28Dec2023")
load("target.25.total.n.30.rslt_2023_12_28_15_40_53_PST_4352.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.25
all.sim.rslt <- tmp

load("target.25.total.n.30.rslt_2023_12_28_15_49_10_PST_811.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.25
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 30

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
# 3, 0, 2, 3, 6, 1, 4, 4, 9, 1, 5, 5, 12, 2, 6, 6 
# 25384 
# 3, 0, 2, 3, 6, 1, 3, 4, 9, 1, 4, 5, 12, 2, 5, 6 
# 21987 
# 3, 0, 2, 3, 6, 0, 4, 4, 9, 1, 5, 5, 12, 1, 6, 6 
# 18170 
# 3, 0, 1, 3, 6, 1, 2, 4, 9, 1, 3, 5, 12, 2, 4, 6 
# 17150 
# 3, 0, 2, 3, 6, 0, 3, 4, 9, 1, 4, 5, 12, 1, 5, 6 
# 15933 
# 3, 0, 2, 3, 6, 1, 3, 4, 9, 1, 5, 5, 12, 2, 6, 6 
# 12545 
save(boundary.rslt.tb, file="target.25_all_30_possible_boundary_tables_29Dec2023.RData")
all.sim.rslt.target.25 <- all.sim.rslt
## ---------------:
load("target.25_all_30_possible_boundary_tables_29Dec2023.RData")
target.25.summary <- data.frame(boundary = names(boundary.rslt.tb))
target.25.summary$p.saf.min <- sapply(target.25.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.25$my.p.saf[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(min(p.saf.v))
})
target.25.summary$p.saf.min

target.25.summary$p.saf.max <- sapply(target.25.summary$boundary, function(x){
  p.saf.v <- all.sim.rslt.target.25$my.p.saf[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(max(p.saf.v))
})
target.25.summary$p.saf.max

target.25.summary$p.tox.min <- sapply(target.25.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.25$my.p.tox[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(min(p.tox.v))
})
target.25.summary$p.tox.min

target.25.summary$p.tox.max <- sapply(target.25.summary$boundary, function(x){
  p.tox.v <- all.sim.rslt.target.25$my.p.tox[which(all.sim.rslt.target.25$boundary.rslt==x)]
  return(max(p.tox.v))
})
target.25.summary$p.tox.max
## ----------------------:
length(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
# [1] 6
sort(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
# [1] 0.000 0.012 0.033 0.101 0.196 0.210

length(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))
# [1] 7
sort(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))
# [1] 0.29 0.42 0.60 0.65 0.75 0.95 1.00
target.25.summary$sim.freq <- boundary.rslt.tb
target.25.summary$target <- 0.25
target.25.summary$ncohort <- 10
target.25.summary$cohortsize <- 3
target.25.summary$n.earlystop <- 12

sum(target.25.summary$freq)
# [1] 200002
dim(target.25.summary)
# [1] 30  10
save(target.25.summary, file="earlystop12_target.25_all30tables_summary_29Dec2023.RData")

target.25.summary <- target.25.summary[order(round(target.25.summary$p.saf.min, 3), round(target.25.summary$p.tox.min,2)),]
write.csv(target.25.summary, file = "earlystop12_target.25_all30tables_summary_29Dec2023.csv", row.names = F, na = "")

## ------- : 
# plot(1:100,1:100, xaxt="n", yaxt="n")
# axis(side=1,at=c(1,20,30,50),labels=c("1975","1980","1985","1990"))
# axis(side=2,at=c(1,20,30,50),labels=c("1975","1980","1985","1990"))
p.saf.ticks <- sort(unique(round(c(target.25.summary$p.saf.min, target.25.summary$p.saf.max), 3)))
# [1] 0.000 0.012 0.033 0.101 0.196 0.210
p.tox.ticks <- sort(unique(round(c(target.25.summary$p.tox.min, target.25.summary$p.tox.max),2)))
# [1] 0.29 0.42 0.60 0.65 0.75 0.95 1.00

## --------------------------------------------------
# save(my.color, file="my_color_v3_28Dec2023.Rata")
load("my_color_v3_28Dec2023.Rata")
all.sim.rslt <- all.sim.rslt.target.25
plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     xaxt="n", yaxt="n",  
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n30 BOIN boundary tables indicated by different colors\n (target DLT rate = 25%, n.earlystop = 12)\n")
axis(side=1,at=p.tox.ticks,labels=p.tox.ticks) ## x-axis 
axis(side=2,at=p.saf.ticks,labels=p.saf.ticks)
## saved as "target.25.earlystop.12.total.n.30.rslt_sim_20002_29Dec2023.pdf" (10 x 8)
## missing ticks' labels (y = 0.012 & 0.210) can be found in "earlystop12_target.25_all30tables_summary_29Dec2023.xlsx"






## ----------------------- target.30.earlystop.12
setwd("/Users/rlu/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_28Dec2023")
load("target_30_total.n.30.rslt_2023_12_29_00_31_44_PST_4352.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 4352
tmp$target.tox <- 0.3
all.sim.rslt <- tmp

load("target_30_total.n.30.rslt_2023_12_29_00_25_41_PST_811.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
tmp$seed <- 811
tmp$target.tox <- 0.3
all.sim.rslt <- rbind(all.sim.rslt, tmp)
dim(all.sim.rslt)
# [1] 200002      5
length(unique(all.sim.rslt$boundary.rslt))
# [1] 36

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
save(boundary.rslt.tb, file="target.30_all_36_possible_boundary_tables_29Dec2023.RData")

# save(my.color, file="my_color_v3_28Dec2023.Rata")
# load("my_color_v3_28Dec2023.Rata")
my.color.n36 <- c(my.color, colors()[470:476+50])
save(my.color.n36, file="my.color.n36_29Dec2023.RData")

plot(all.sim.rslt$my.p.tox, all.sim.rslt$my.p.saf, 
     col=my.color.n36[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     pch=".",
     ylab="\n\n p.saf", xlab="p.tox \n", 
     main="\n36 BOIN boundary tables indicated by different colors\n (target DLT rate = 30%)\n")
## saved as "target.30.earlystop.12.total.n.30.rslt_sim_20002_29Dec2023.pdf" (8 x 6)

all.sim.rslt.target.30 <- all.sim.rslt

table(unique(all.sim.rslt.target.30$boundary.rslt) %in% unique(all.sim.rslt.target.25$boundary.rslt))
# FALSE 
# 36
table(unique(all.sim.rslt.target.25$boundary.rslt) %in% unique(all.sim.rslt.target.30$boundary.rslt))
# FALSE 
# 30







## ------------- work of 12/29/2023 ends by this line ----------







## ---------------------- reference code from "BOIN_earlystop_n12_explore_28Dec2023_analysis.R":
tb1.p.saf.p.tox <- all.sim.rslt[which(all.sim.rslt$boundary.rslt=="3, 0, 1, 3, 6, 1, 2, 4, 9, 1, 3, 5, 12, 2, 4, 6"),]
dim(tb1.p.saf.p.tox)
# [1] 3010    3
head(tb1.p.saf.p.tox[,-3])
#    my.p.saf  my.p.tox
# 5  0.1309354 0.4012432
# 11 0.1652023 0.3627906
# 14 0.1415699 0.3358606
# 18 0.1471670 0.4092458
# 19 0.1197237 0.3061285
# 21 0.1045569 0.3354658
plot(tb1.p.saf.p.tox$my.p.saf, tb1.p.saf.p.tox$my.p.tox)

plot(all.sim.rslt$my.p.saf, all.sim.rslt$my.p.tox, col=factor(all.sim.rslt$boundary.rslt))
## saved as "target.25.total.n.30.rslt_2023_12_28_14_06_19_PST_420.pdf"

## -----------------------
load("target.25.total.n.30.rslt_2023_12_28_15_04_14_PST_187.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 10001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 10001    3
all.sim.rslt <- tmp
my.color <- colors()[c(5,12,17,23,24,
                       30,36,41,46,51,
                       56,61,67,72,79,
                       80,81,89,94,99,
                       100,101,106,112,120,
                       125,132,137,146,151)-2]
my.color <- my.color[sample(1:length(my.color),length(my.color),replace = F)]
save(my.color, file="my_color_v1_28Dec2023.Rata")
my.color <- my.color[sample(1:length(my.color),length(my.color),replace = F)]
save(my.color, file="my_color_v2_28Dec2023.Rata")

plot(1:30, 1:30, col=my.color, pch=19, cex=2)
my.color[5] <- "black"
my.color[2] <- "red"
my.color[4] <- "green"
my.color[18] <- "yellow"
my.color[8] <- "grey"
my.color[30] <- "pink"
my.color[10] <- my.color[2]
my.color[2] <- "darkgreen"
my.color[26] <- "yellow3"
my.color <- my.color[sample(1:length(my.color),length(my.color),replace = F)]
save(my.color, file="my_color_v3_28Dec2023.Rata")
plot(all.sim.rslt$my.p.saf, all.sim.rslt$my.p.tox, 
     col=my.color[as.integer(factor(all.sim.rslt$boundary.rslt))], 
     pch=20)
## saved as "target.25.total.n.30.rslt_2023_12_28_15_04_14_PST_187.pdf"

boundary.rslt.tb <- sort(table(all.sim.rslt$boundary.rslt),decreasing = T)
boundary.rslt.tb
length(boundary.rslt.tb)







## --------------------- reference:
load("target.25.total.n.30.rslt_2023_12_28_11_51_49_PST_8043.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 20001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 20001    3
all.sim.rslt <- rbind(all.sim.rslt, tmp)

load("target.25.total.n.30.rslt_2023_12_28_11_53_51_PST_7761.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 30001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 30001    3
all.sim.rslt <- rbind(all.sim.rslt, tmp)

load("target.25.total.n.30.rslt_2023_12_28_12_00_28_PST_912.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 50001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 50001    3
all.sim.rslt <- rbind(all.sim.rslt, tmp)

load("target.25.total.n.30.rslt_2023_12_28_12_21_15_PST_420.RData")
dim(saved.target.25.total.n.30.rslt)
# [1] 100001    3
tmp <- unique(saved.target.25.total.n.30.rslt)
dim(tmp)
# [1] 100001    3
all.sim.rslt <- rbind(all.sim.rslt, tmp)

dim(all.sim.rslt)
# [1] 210005     3







