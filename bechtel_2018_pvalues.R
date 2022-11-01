###################################################
## arquivo: bechtel_2018_pvalues.R
## Monografia I
## Replicação Bechtel et al. (2018)
## Luigi Garzon
## Este código calcula o p-valor para as diferentes
## especificações da replicação
###################################################

rm(list = ls(all= TRUE))

setwd("C:/Users/Luigi/Dropbox/EESP/9 Semestre/Monografia/Code/Replications/Bechtel et al. (2018)")

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

specs <- read.csv("specs.csv")[,-1]


####################################################
## P-values
## p := \sum_{j=1}^{J+1}1[RMSPE_j \geq RMSPE_22]/(J+1)
####################################################


rmspe_22 <- specs %>% filter(V2 == 22) %>% select(V3, spec_n)
specs <- inner_join(specs, rmspe_22, by = "spec_n")
specs <- specs %>% rename(rmspe22 = V3.y)
specs <- specs %>% rename(rmspe = V3.x)

specs <- specs %>% mutate(rmspe_ind = ifelse(rmspe >= rmspe22, 1, 0))

specs %>% select(V2,rmspe, rmspe22)

specs %>% group_by(spec_n) %>% summarise(pvalue = sum(rmspe_ind)*(1/15))



