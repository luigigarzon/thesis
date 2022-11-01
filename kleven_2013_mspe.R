###################################################
## arquivo: kleven_2013_mspe.R
## Monografia I
## Replicação Kleven et al. (2013)
## Luigi Garzon
## Este código constroi os gráficos do SC
###################################################

rm(list = ls(all= TRUE))

setwd("C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Code/Replications/Kleven et al. (2013)")

# Packages ----------------------------------------------------------------

library(Synth)
library(tidyverse)
library(foreign)
library("doParallel")
library("data.table")
library("statar")
library("readstata13")
library(purrr)

# Data --------------------------------------------------------------------

specs <- read.csv("specs_kleven.csv")[,-1]

for (i in c(1:8)) {
  assign(paste0("spec_",LETTERS[i],"1"), specs %>% filter(spec_n == paste0(LETTERS[i], "_1")))
  assign(paste0("spec_",LETTERS[i],"2"), specs %>% filter(spec_n == paste0(LETTERS[i], "_2")))
  
}

# MSPE --------------------------------------------------------------------

MSPE = matrix(0, 16, 1)


# Compute the MSPE for each specification
for (i in c(1:8)) {
  for (j in c(1:5,7:14)) {
    MSPE[i,1] = MSPE[i,1] + as.numeric(get(paste0("spec_",LETTERS[i],"1"))[j, 25])
    MSPE[i+8,1] = MSPE[i+8,1] + as.numeric(get(paste0("spec_",LETTERS[i],"2"))[j, 25])
  }
}

MSPE = MSPE/13

specs_n = matrix(0, 16, 1)
for (i in c(1:8)) {
  specs_n[i, 1] = paste0(LETTERS[i],"1")
  specs_n[i+8, 1] = paste0(LETTERS[i],"2")
  
}

MSPE <- cbind(MSPE, specs_n)

min(MSPE)
which(MSPE == min(MSPE))
