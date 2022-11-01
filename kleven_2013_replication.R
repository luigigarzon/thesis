###################################################
## arquivo: kleven_2013_replication.R
## Monografia I
## Replicação Kleven et al. (2013)
## Luigi Garzon
## Este código constroi os gráficos do SC
###################################################

rm(list = ls(all= TRUE))

setwd("C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Data/Kleven et al. (2013)/Data_-_Prog_AER/data/data_graph")

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

df <- read.dta("basesynthetic_control.dta")
df <- df %>% filter(year >= 1990)

# Replication -------------------------------------------------------------

#######################################################
# Specifications
#######################################################


# Specification A
# All pre-treatment outcome values
#######################################################


spec_A <- list() 
for(i in c(1990:2003)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_A[[i]] <- y_lags
}

spec_A[sapply(spec_A , is.null)] <- NULL

# Specification B
# First 3/4 of the pre-treatment outcome values (1990:2000)
#######################################################

spec_B <- list() 
for(i in c(1990:2000)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_B[[i]] <- y_lags
}

spec_B[sapply(spec_B , is.null)] <- NULL

# Specification C
# Odd pre-treatment values
#######################################################

spec_C <- list() 
for(i in seq(from = 1990, to = 2003, by = 2)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_C[[i]] <- y_lags
}

spec_C[sapply(spec_C , is.null)] <- NULL

# Specification D
# Even pre-treatment values
#######################################################

spec_D <- list() 
for(i in seq(from = 1991, to = 2003, by = 2)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_D[[i]] <- y_lags
}

spec_D[sapply(spec_D , is.null)] <- NULL

# Specification E
# Outcome Mean
#######################################################

spec_E <- list(
  list("treattop", seq(from = 1990, to = 2003, by = 1), "mean")
)

# Specification F
# First half of the pre-treatment outcome values (1990:1997)
#######################################################

spec_F <- list() 
for(i in c(1990:1997)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_F[[i]] <- y_lags
}

spec_F[sapply(spec_F , is.null)] <- NULL

# Specification G
# Three pre-treatment values (1,16,32)
#######################################################

spec_G <- list() 
for(i in c(1990, 1997, 2003)){
  y_lags <- list("treattop", seq(from = i, to = i, by = 1), "mean")
  spec_G[[i]] <- y_lags
}

spec_G[sapply(spec_G , is.null)] <- NULL

# Specification H
# Three pre-treatment values (1,16,32)
#######################################################

spec_H <- list(
  list("treattop", c(1990:1997), "mean"),
  list("treattop", c(1998:2003), "mean"))


#######################################################
## Set up
#######################################################

covariates = c("quality", "country_sumtotpts_rel", "uefa_coef_1")

#######################################################
## Synthetic Controls
#######################################################

# Specification with covariates


for (i in c(1:8)) {
  assign(paste0("spec_matrix_", LETTERS[i], "_1"), foreach(j=c(1:14), .combine = rbind, .packages = "Synth") %do% {
    # Define the comparison regions.
    controlunits <- setdiff(c(1:14), j)
    # Prepare the data in order to use the synthetic control estimator
    dataprep.out <- dataprep(
      foo = df,
      predictors = covariates,
      predictors.op = "mean",
      time.predictors.prior = c(1990:2003),
      special.predictors = get(paste0("spec_", LETTERS[i])),
      dependent = "treattop",
      unit.variable = "id",
      unit.names.variable = "country",
      time.variable = "year",
      treatment.identifier = j,
      controls.identifier = controlunits,
      time.optimize.ssr = c(1990:2003),
      time.plot = c(1990:2008))
    # Estimating the synthetic control method
    print(paste0("Specification ", LETTERS[i], "1 - Unit: ", j))
    synth.out <- try(synth(data.prep.obj = dataprep.out, method = "Nelder-Mead"))
    if (inherits(synth.out,"try-error")){
      print("retry")
      synth.out <-synth(data.prep.obj = dataprep.out, optimxmethod = "Nelder-Mead",quadopt = "LowRankQP")
    }
    gaps<-dataprep.out$Y1plot - dataprep.out$Y0plot%*%synth.out$solution.w
    pre<-t(gaps[1:14])%*%gaps[1:14]/length(gaps[1:14])
    post<-t(gaps[15:19])%*%gaps[15:19]/length(gaps[15:19])
    rrmspe<-post/pre
    pre.gaps<-gaps[1:14]
    num<-t(pre.gaps)%*%pre.gaps
    devs<-dataprep.out$Y1plot[1:14]-mean(dataprep.out$Y1plot[1:14])
    den<-t(devs)%*%devs
    RSSR<-num/den
    Y1 <- dataprep.out$Y1plot
    Y0 <- dataprep.out$Y0plot%*%synth.out$solution.w
    print("Everything is fine")
    return(c("mean",j,rrmspe,RSSR, gaps, "blank", post, Y1, Y0))})
}

# Specification without covariates


for (i in c(1:8)) {
  assign(paste0("spec_matrix_", LETTERS[i], "_2"), foreach(j=c(1:14), .combine = rbind, .packages = "Synth") %do% {
    # Define the comparison regions.
    controlunits <- setdiff(c(1:14), j)
    # Prepare the data in order to use the synthetic control estimator
    dataprep.out <- dataprep(
      foo = df,
      predictors = NULL,
      predictors.op = "mean",
      time.predictors.prior = c(1990:2003),
      special.predictors = get(paste0("spec_", LETTERS[i])),
      dependent = "treattop",
      unit.variable = "id",
      unit.names.variable = "country",
      time.variable = "year",
      treatment.identifier = j,
      controls.identifier = controlunits,
      time.optimize.ssr = c(1990:2003),
      time.plot = c(1990:2008))
    # Estimating the synthetic control method
    print(paste0("Specification ", LETTERS[i], "2 - Unit: ", j))
    synth.out <- try(synth(data.prep.obj = dataprep.out, method = "Nelder-Mead"))
    if (inherits(synth.out,"try-error")){
      print("retry")
      synth.out <-synth(data.prep.obj = dataprep.out, optimxmethod = "Nelder-Mead",quadopt = "LowRankQP")
    }
    gaps<-dataprep.out$Y1plot - dataprep.out$Y0plot%*%synth.out$solution.w
    pre<-t(gaps[1:14])%*%gaps[1:14]/length(gaps[1:14])
    post<-t(gaps[15:19])%*%gaps[15:19]/length(gaps[15:19])
    rrmspe<-post/pre
    pre.gaps<-gaps[1:14]
    num<-t(pre.gaps)%*%pre.gaps
    devs<-dataprep.out$Y1plot[1:14]-mean(dataprep.out$Y1plot[1:14])
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

write.csv(specs, "C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Code/Replications/Kleven et al. (2013)/specs_kleven.csv")
