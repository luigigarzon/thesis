###################################################
## arquivo: bechtel_2018_replication.R
## Monografia I
## Replicação Bechtel et al. (2018)
## Luigi Garzon
## Este código replica o paper Bechtel et al. (2018)
## para 16 especificações diferentes
###################################################

rm(list = ls(all= TRUE))

setwd("C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Data/Bechtel et al. (2018)/Replication Archive BHS 2017")

# Packages ----------------------------------------------------------------

library(Synth)
library(tidyverse)
library(foreign)
library("doParallel")
library("data.table")
library("statar")
library("readstata13")
library(purrr)
library(haven)
library(devtools)
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)


# Data --------------------------------------------------------------------

df <- read.dta("data1.dta")


# Replication -------------------------------------------------------------


#######################################################
# Specifications
#######################################################

# Specification A
# All pre-treatment outcome values
#######################################################


spec_A <- list() 
for(i in c(1:32)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_A[[i]] <- y_lags
}

spec_A[sapply(spec_A , is.null)] <- NULL

# Specification B
# First 3/4 of the pre-treatment outcome values (1:24)
#######################################################

spec_B <- list() 
for(i in c(1:24)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_B[[i]] <- y_lags
}

spec_B[sapply(spec_B , is.null)] <- NULL

# Specification C
# Odd pre-treatment values
#######################################################

spec_C <- list() 
for(i in seq(from = 1, to = 32, by = 2)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_C[[i]] <- y_lags
}

spec_C[sapply(spec_C , is.null)] <- NULL

# Specification D
# Even pre-treatment values
#######################################################

spec_D <- list() 
for(i in seq(from = 2, to = 32, by = 2)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_D[[i]] <- y_lags
}

spec_D[sapply(spec_D , is.null)] <- NULL

# Specification E
# Outcome Mean
#######################################################

spec_E <- list(
  list("turnout_mean", seq(from = 1, to = 32, by = 1), "mean")
)

# Specification F
# First half of the pre-treatment outcome values (1:16)
#######################################################

spec_F <- list() 
for(i in c(1:16)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_F[[i]] <- y_lags
}

spec_F[sapply(spec_F , is.null)] <- NULL

# Specification G
# Three pre-treatment values (1,16,32)
#######################################################

spec_G <- list() 
for(i in c(1, 16, 32)){
  y_lags <- list("turnout_mean", seq(from = i, to = i, by = 1), "mean")
  spec_G[[i]] <- y_lags
}

spec_G[sapply(spec_G , is.null)] <- NULL

# Specification H
# Three pre-treatment values (1,16,32)
#######################################################

spec_H <- list(
  list("turnout_mean", c(1:16), "mean"),
  list("turnout_mean", c(17:32), "mean"))


#######################################################
## Set up
#######################################################

vaud_id <- 22
control_units <- c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 23, 24, 25)
covariates <-  c("over_40_i", "over_50_i", "over_60_i", "pub_revenue_pc_i",
                 "pub_spending_pc_i", "log_population_i", "urban_pop_i",
                 "work_pop_pop_i", "work_sec1_pop_i", "work_sec2_pop_i",
                 "motor_pop_i")


#######################################################
## Synthetic Controls
#######################################################

# Specification with covariates


for (i in c(1:8)) {
  assign(paste0("spec_matrix_", LETTERS[i], "_1"), foreach(j=c(2:7,9:13,22:25), .combine = rbind, .packages = "Synth") %do% {
    # Define the comparison regions.
    controlunits <- setdiff(c(2:7,9:13,22:25), j)
    # Prepare the data in order to use the synthetic control estimator
    dataprep.out <- dataprep(
      foo = df,
      predictors = covariates,
      predictors.op = "mean",
      time.predictors.prior = c(1:32),
      special.predictors = get(paste0("spec_", LETTERS[i])),
      dependent = "turnout_mean",
      unit.variable = "id",
      unit.names.variable = "canton",
      time.variable = "dat_num",
      treatment.identifier = j,
      controls.identifier = controlunits,
      time.optimize.ssr = c(1:32),
      time.plot = c(1:130))
    # Estimating the synthetic control method
    print(paste0("Specification ", LETTERS[i], "1 - Unit: ", j))
    synth.out <- try(synth(data.prep.obj = dataprep.out, method = "Nelder-Mead"))
    if (inherits(synth.out,"try-error")){
      print("retry")
      synth.out <-synth(data.prep.obj = dataprep.out, optimxmethod = "Nelder-Mead",quadopt = "LowRankQP")
    }
    gaps<-dataprep.out$Y1plot - dataprep.out$Y0plot%*%synth.out$solution.w
    pre<-t(gaps[1:32])%*%gaps[1:32]/length(gaps[1:32])
    post<-t(gaps[32:130])%*%gaps[32:130]/length(gaps[32:130])
    rrmspe<-post/pre
    pre.gaps<-gaps[1:32]
    num<-t(pre.gaps)%*%pre.gaps
    devs<-dataprep.out$Y1plot[1:32]-mean(dataprep.out$Y1plot[1:32])
    den<-t(devs)%*%devs
    RSSR<-num/den
    Y1 <- dataprep.out$Y1plot
    Y0 <- dataprep.out$Y0plot%*%synth.out$solution.w
    print("Everything is fine")
    return(c("mean",j,rrmspe,RSSR, gaps, "blank", post, Y1, Y0))})
}

# Specifications without covariates

for (i in c(1:8)) {
  assign(paste0("spec_matrix_", LETTERS[i], "_2"), foreach(j=c(2:7,9:13,22:25), .combine = rbind, .packages = "Synth") %do% {
    # Define the comparison regions.
    controlunits <- setdiff(c(2:7,9:13,22:25), j)
    # Prepare the data in order to use the synthetic control estimator
    dataprep.out <- dataprep(
      foo = df,
      predictors = NULL,
      predictors.op = "mean",
      time.predictors.prior = c(1:32),
      special.predictors = get(paste0("spec_", LETTERS[i])),
      dependent = "turnout_mean",
      unit.variable = "id",
      unit.names.variable = "canton",
      time.variable = "dat_num",
      treatment.identifier = j,
      controls.identifier = controlunits,
      time.optimize.ssr = c(1:32),
      time.plot = c(1:130))
    # Estimating the synthetic control method
    print(paste0("Specification ", LETTERS[i], "2 - Unit: ", j))
    synth.out <- try(synth(data.prep.obj = dataprep.out, method = "Nelder-Mead"))
    if (inherits(synth.out,"try-error")){
      print("retry")
      synth.out <-synth(data.prep.obj = dataprep.out, optimxmethod = "Nelder-Mead",quadopt = "LowRankQP")
    }
    gaps<-dataprep.out$Y1plot - dataprep.out$Y0plot%*%synth.out$solution.w
    pre<-t(gaps[1:32])%*%gaps[1:32]/length(gaps[1:32])
    post<-t(gaps[32:130])%*%gaps[32:130]/length(gaps[32:130])
    rrmspe<-post/pre
    pre.gaps<-gaps[1:32]
    num<-t(pre.gaps)%*%pre.gaps
    devs<-dataprep.out$Y1plot[1:32]-mean(dataprep.out$Y1plot[1:32])
    den<-t(devs)%*%devs
    RSSR<-num/den
    Y1 <- dataprep.out$Y1plot
    Y0 <- dataprep.out$Y0plot%*%synth.out$solution.w
    print("Everything is fine")
    return(c("mean",j,rrmspe,RSSR, gaps, "blank", post, Y1, Y0))})
}


specs <- list()
spec_matrices <- outer("_", c(1:2), paste0)
spec_matrices <- outer(LETTERS[1:8], spec_matrices, paste0)


for (i in c(1:16)) {
  specs[[i]] <- get(paste0("spec_matrix_", spec_matrices[i]))
  
}  

specs <- Map(cbind, specs, spec_n = spec_matrices)
specs <- lapply(specs, as.data.frame)
specs <- bind_rows(specs)

write.csv(specs, "C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Code/Replications/Bechtel et al. (2018)/specs.csv")



