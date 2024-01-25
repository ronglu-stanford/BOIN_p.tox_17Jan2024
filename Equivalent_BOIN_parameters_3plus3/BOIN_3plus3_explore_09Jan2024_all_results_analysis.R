rm(list=ls())
setwd("~/Downloads/BOIN_vs_mTPI_vs_mTPI-2_10102023/IWSM_May2024_meeting_due01302024/BOIN_simulation_09Jan2024")
#install.packages("BOIN")
library(BOIN)

load("BOIN_3plus3_explore_09Jan2024_2024_01_10_05_27_28_PST_4352.RData")
all.saved.rslt <- saved.rslt
dim(all.saved.rslt)
# [1] 179   6

filenames <- list.files("./", pattern="*.RData", full.names=TRUE)
for(i in filenames){
  load(i)
  all.saved.rslt <- rbind(all.saved.rslt, saved.rslt)
}
dim(all.saved.rslt)
# [1] 8362    6

boundary.3plus3 <- paste(t(c(3, 0, 2, 2, 6, 1, 2, 2)), collapse=", ")
table(all.saved.rslt$boundary.rslt==boundary.3plus3)
# FALSE  TRUE 
#    57  8305 
all.saved.rslt <- all.saved.rslt[-which(all.saved.rslt$boundary.rslt!=boundary.3plus3),]
dim(all.saved.rslt)
# [1] 8305    6
all.saved.rslt <- unique(all.saved.rslt)
dim(all.saved.rslt)
# [1] 8127    6
save(all.saved.rslt, file="BOIN_3plus3_explore_09Jan2024_all_results_r8127c6.RData")

plot(all.saved.rslt$target, all.saved.rslt$my.p.tox)
plot(all.saved.rslt$target, all.saved.rslt$my.p.saf)
plot(all.saved.rslt$target, all.saved.rslt$test.cutoff)

plot(all.saved.rslt$target, all.saved.rslt$test.offset)

summary(all.saved.rslt$target)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1761  0.2148  0.2345  0.2344  0.2545  0.2903
summary(all.saved.rslt$my.p.saf)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.08186 0.14004 0.16025 0.16176 0.18209 0.25663
summary(all.saved.rslt$my.p.tox)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3884  0.5868  0.7222  0.7226  0.8633  1.0000

head(all.saved.rslt[which(all.saved.rslt$my.p.tox>0.99),])
#        target  my.p.saf  my.p.tox test.cutoff test.offset          boundary.rslt
# 58  0.2199837 0.1657162 0.9978488   0.8038428        0.01 3, 0, 2, 2, 6, 1, 2, 2
# 70  0.2645619 0.1385043 0.9962199   0.7243473        0.01 3, 0, 2, 2, 6, 1, 2, 2
# 174 0.2045848 0.1511395 0.9923790   0.8270433        0.01 3, 0, 2, 2, 6, 1, 2, 2
# 397 0.1838882 0.1546687 0.9994928   0.8685382        0.01 3, 0, 2, 2, 6, 1, 2, 2
# 400 0.2491100 0.1779963 0.9935578   0.7566956        0.01 3, 0, 2, 2, 6, 1, 2, 2
# 426 0.2138343 0.1756644 0.9942594   0.8225117        0.01 3, 0, 2, 2, 6, 1, 2, 2
get.boundary(target=0.1838882, ncohort=10, cohortsize=3, n.earlystop = 6,
             p.saf = 0.1546687, p.tox = 0.9994928, cutoff.eli = 0.8685382,
             extrasafe = T, offset = 0.01)
# $lambda_e
# [1] 0.1689429
# 
# $lambda_d
# [1] 0.8134795
# 
# $boundary_tab
# 
# Number of patients treated 3 6
# Escalate if # of DLT <=    0 1
# Deescalate if # of DLT >=  2 2
# Eliminate if # of DLT >=   2 2
all.saved.rslt$ncohort <- 10
all.saved.rslt$cohortsize <- 3
all.saved.rslt$n.earlystop <- 6
dim(all.saved.rslt)
# [1] 8127    9
save(all.saved.rslt, file="BOIN_3plus3_explore_09Jan2024_all_results_r8127c9.RData")
write.csv(all.saved.rslt, file="BOIN_3plus3_explore_09Jan2024_all_results_r8127c9.csv", row.names = F, na="")

rounded.rslt <- all.saved.rslt
rounded.rslt$target <- round(rounded.rslt$target, 3)
rounded.rslt$my.p.saf <- round(rounded.rslt$my.p.saf, 3)
rounded.rslt$my.p.tox <- round(rounded.rslt$my.p.tox, 3)
rounded.rslt$test.cutoff <- round(rounded.rslt$test.cutoff, 3)

boundary.data <- data.frame(target=sort(unique(rounded.rslt$target)))
boundary.data$max.p.saf <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$my.p.saf[which(rounded.rslt$target==x)]
  return(max(sub.data))
})
boundary.data$min.p.saf <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$my.p.saf[which(rounded.rslt$target==x)]
  return(min(sub.data))
})
boundary.data$max.p.tox <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$my.p.tox[which(rounded.rslt$target==x)]
  return(max(sub.data))
})
boundary.data$min.p.tox <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$my.p.tox[which(rounded.rslt$target==x)]
  return(min(sub.data))
})
boundary.data$max.cutoff.eli <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$test.cutoff[which(rounded.rslt$target==x)]
  return(max(sub.data))
})
boundary.data$min.cutoff.eli <- sapply(boundary.data$target, function(x){
  sub.data <- rounded.rslt$test.cutoff[which(rounded.rslt$target==x)]
  return(min(sub.data))
})


library(ggplot2)
ggplot(boundary.data, aes(x=target, y=max.p.saf)) + geom_point()
ggplot(all.saved.rslt, aes(x=target, y=my.p.saf)) + geom_point() + stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

summary(glm(max.p.saf ~ target, data=boundary.data))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.016893   0.004896    3.45 0.000788 ***
# target      0.816618   0.020804   39.25  < 2e-16 ***
summary(glm(min.p.saf ~ target, data=boundary.data))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.262973   0.003354   78.41   <2e-16 ***
# target      -0.631421   0.014251  -44.31   <2e-16 ***
boundary.data$target.sq <- boundary.data$target^2
summary(glm(min.p.saf ~ target + target.sq, data=boundary.data))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.45026    0.01872   24.05   <2e-16 ***
# target      -2.27236    0.16293  -13.95   <2e-16 ***
# target.sq    3.52133    0.34892   10.09   <2e-16 ***
boundary.data$target3 <- boundary.data$target^3
summary(glm(min.p.saf ~ target + target.sq + target3, data=boundary.data))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.1035     0.1379  -0.750 0.454766    
# target        5.0373     1.8123   2.779 0.006396 ** 
# target.sq   -28.2374     7.8529  -3.596 0.000484 ***
# target3      45.4345    11.2247   4.048  9.6e-05 ***


## ---------------------------
ggplot(all.saved.rslt, aes(x=target, y=my.p.saf, colour=my.p.tox)) + geom_point() + 
  geom_abline(intercept = 0.002, slope = 0.9, color="red", linetype="dashed", size=1.5) + 
  geom_abline(intercept = 0.306, slope = -0.85, color="green", linetype="dashed", size=1) + 
  stat_function(fun=function(x) 0.4295 - 2.11*x + 3.1171*x^2, color="red", linetype="dashed", size=1.5) + 
  labs(colour = "BOIN p.tox") + labs(x = "\n BOIN target DLT rate\n\n", y="\n\n BOIN p.saf\n", title="\n Under the same 3+3 design boundary table: \n")
## saved as "BOIN_3plus3_target_vs_p.saf_figure_09Jan2024.pdf" (8 x 6)


summary(glm(min.p.tox ~ target, data=boundary.data))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.72523    0.01535   47.24   <2e-16 ***
# target      -1.13960    0.06523  -17.47   <2e-16 ***

## ---------------------------
ggplot(all.saved.rslt, aes(x=target, y=my.p.tox, colour=my.p.saf)) + geom_point() + 
  geom_abline(intercept = 1, slope = 0, color="red", linetype="dashed", size=1.5) + 
  geom_abline(intercept = 0.72523, slope = -1.2, color="red", linetype="dashed", size=1.5) + 
  labs(colour = "BOIN p.saf") + labs(x = "\n BOIN target DLT rate\n\n", y="\n\n BOIN p.tox\n", title="\n Under the same 3+3 design boundary table: \n")
## saved as "BOIN_3plus3_target_vs_p.tox_figure_09Jan2024.pdf" (8 x 6)


summary(glm(min.cutoff.eli ~ target, data=boundary.data))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.1490196  0.0009456    1215   <2e-16 ***
# target      -1.6471080  0.0040176    -410   <2e-16 ***
summary(glm(max.cutoff.eli ~ target, data=boundary.data))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.235502   0.003125   395.3   <2e-16 ***
# target      -1.925274   0.013280  -145.0   <2e-16 ***
summary(glm(min.cutoff.eli ~ target + target.sq, data=boundary.data))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.088184   0.004422  246.08   <2e-16 ***
# target      -1.114098   0.038492  -28.94   <2e-16 ***
# target.sq   -1.143798   0.082435  -13.88   <2e-16 ***
summary(glm(max.cutoff.eli ~ target + target.sq, data=boundary.data))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.01674    0.01209  84.074   <2e-16 ***
# target      -0.00858    0.10527  -0.082    0.935    
# target.sq   -4.11308    0.22544 -18.245   <2e-16 ***
## ---------------------------
ggplot(all.saved.rslt, aes(x=target, y=test.cutoff, colour=my.p.saf)) + geom_point() + 
  #geom_abline(intercept = 1.145, slope = -1.6471, color="red", linetype="dashed", size=1.5) + 
  #geom_abline(intercept = 1.25, slope = -1.95, color="green", linetype="dashed", size=1) + 
  stat_function(fun=function(x) 1.0215 - 0.00858*x - 4.11*x^2, color="red", linetype="dashed", size=1.5) + ## upper bound
  stat_function(fun=function(x) 1.085 - 1.1141*x - 1.1438*x^2, color="red", linetype="dashed", size=1.5) + 
  labs(colour = "BOIN p.saf") + labs(x = "\n BOIN target DLT rate\n\n", y="\n\n BOIN cutoff.eli\n", title="\n Under the same 3+3 design boundary table: \n")
## saved as "BOIN_3plus3_target_vs_cutoff.eli_figure_09Jan2024.pdf" (8 x 6)

save.image("BOIN_3plus3_explore_09Jan2024_all_results_analysis_image.RData")

hist(all.saved.rslt$target)
hist(all.saved.rslt$my.p.saf)
hist(all.saved.rslt$my.p.tox)
hist(all.saved.rslt$test.cutoff)

summary(all.saved.rslt$target)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.1761  0.2148  0.2345  0.2344  0.2545  0.2903
summary(all.saved.rslt$my.p.saf)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.08186 0.14004 0.16025 0.16176 0.18209 0.25663
summary(all.saved.rslt$my.p.tox)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.3884  0.5868  0.7222  0.7226  0.8633  1.0000
summary(all.saved.rslt$test.cutoff)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.6689  0.7384  0.7752  0.7746  0.8109  0.8886

## -------------------------
all.saved.rslt[which(all.saved.rslt$target==min(all.saved.rslt$target)),]
#         target  my.p.saf my.p.tox test.cutoff test.offset          boundary.rslt ncohort cohortsize n.earlystop
# 8310 0.1761482 0.1582749 0.892814   0.8548338        0.49 3, 0, 2, 2, 6, 1, 2, 2      10          3           6
get.boundary(target=0.1761482, ncohort=10, cohortsize=3, n.earlystop = 6,
             p.saf = 0.1582749, p.tox = 0.892814, cutoff.eli = 0.8548338,
             extrasafe = T, offset = 0.49)

all.saved.rslt[which(all.saved.rslt$target==max(all.saved.rslt$target)),]
#         target  my.p.saf  my.p.tox test.cutoff test.offset          boundary.rslt ncohort cohortsize n.earlystop
# 7277 0.2903009 0.1501848 0.4812773   0.6689076         0.2 3, 0, 2, 2, 6, 1, 2, 2      10          3           6
get.boundary(target=0.2903009, ncohort=10, cohortsize=3, n.earlystop = 6,
             p.saf = 0.1501848, p.tox = 0.4812773, cutoff.eli = 0.6689076,
             extrasafe = T, offset = 0.2)
