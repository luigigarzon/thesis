###################################################
## arquivo: kleven_2013_plots.R
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

Y1 <- specs %>% select(c(2, 26:44, 64)) %>% filter(V2 == 6) %>% filter(spec_n == "A_1")
Y1 <- Y1[, -c(1,21)]
Y1 <- as.data.frame(t(Y1))

Y0 <- specs %>% select(c(2, 45:64)) %>% filter(V2 == 6) %>% select(-V2)
Y0 <- as.data.frame(t(Y0))
colnames(Y0) <- Y0[20,]
Y0 <- Y0[-20,]
Y0 <- lapply(Y0, as.numeric)

graphs <- cbind(Y1,Y0)
colnames(graphs)[1] <- "Y1" 
graphs$year <- c(1990:2008)

# Graphs ------------------------------------------------------------------

graphs %>% ggplot(aes(x = year)) +
  geom_line(aes(y=Y1, color = "Spain"), size = 1) +
  geom_line(aes(y=A_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=B_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=C_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=D_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=E_1), linetype = "dashed", color = "red") +
  geom_line(aes(y=F_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=G_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=H_1), linetype = "dashed", color = "grey") +
  geom_line(aes(y=A_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=B_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=C_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=D_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=E_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=F_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=G_2), linetype = "dashed", color = "grey") +
  geom_line(aes(y=H_2), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  theme_bw() +
  labs(title = "Top Players", subtitle = "", y = "Share of foreign players", x = "Year") +
  scale_color_manual(name = "", breaks = c("Spain", "Original Specification", "Other Specifications"),
                     values = c("Spain" = "black", "Original Specification" = "red", "Other Specifications" = "grey")) +
  theme(legend.position="bottom", legend.box = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
