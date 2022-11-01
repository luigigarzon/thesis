###################################################
## arquivo: kleven_2013_pvalues.R
## Monografia I
## Replicação Kleven et al. (2013)
## Luigi Garzon
## Este código calcula o p-valor para as diferentes
## especificações da replicação
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


####################################################
## P-values
## p := \sum_{j=1}^{J+1}1[RMSPE_j \geq RMSPE_22]/(J+1)
####################################################


rmspe_6 <- specs %>% filter(V2 == 6) %>% select(V3, spec_n)
specs <- inner_join(specs, rmspe_6, by = "spec_n")
specs <- specs %>% rename(rmspe6 = V3.y)
specs <- specs %>% rename(rmspe = V3.x)

specs <- specs %>% mutate(rmspe_ind = ifelse(rmspe >= rmspe6, 1, 0))

specs %>% select(V2,rmspe, rmspe6)

specs %>% group_by(spec_n) %>% summarise(pvalue = sum(rmspe_ind)*(1/14))

