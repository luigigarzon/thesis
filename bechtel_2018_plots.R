###################################################
## arquivo: bechtel_2018_plots.R
## Monografia I
## Replicação Bechtel et al. (2018)
## Luigi Garzon
## Este código constroi os gráficos do SC
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

Y1 <- specs %>% select(c(2, 137:266, 397)) %>% filter(V2 == 22) %>% filter(spec_n == "A_1")
Y1 <- Y1[, -c(1,132)]

Y0 <- specs %>% select(c(2, 267:397)) %>% filter(V2 == 22) %>% select(-V2, -spec_n)

# Graphs ------------------------------------------------------------------


X = c(1:130)

yearcutoffs <- c(1,11,21,42,59,74,106,127,130)
X_equalspace <- matrix(NA,length(X),1)
for (i in 2:length(yearcutoffs)){
  X_min <- min(X[is.element(X,yearcutoffs[i]:yearcutoffs[(i-1)])])
  X_max <- max(X[is.element(X,yearcutoffs[i]:yearcutoffs[(i-1)])])
  X_dist <- X_max-X_min 
  X_equalspace[is.element(X,yearcutoffs[i]:yearcutoffs[(i-1)])]  <- (i-1)+ (X[is.element(X,yearcutoffs[i]:yearcutoffs[(i-1)])]-X_min)/X_dist 
}

X_equal_10years <- X_equalspace[is.element(X,yearcutoffs)]

X_equalspace[c(128:130)] <- (length(yearcutoffs)-1)+(X_equalspace[127]-X_equalspace[126])*c(1:3)

gray.scale = .7
gray.scale2 = .7
dim.factor = .1
Legend = c("Vaud                  ","Synthetic Vaud")
Ylab = c("Turnout (%)")
Xlab = c("Year") 
Ylim = c(0,100) 
Xlim = c(min(X_equalspace),max(X_equalspace))
plot(X_equalspace, Y1*100, t = "l", col = "white", 
     lwd = 1,  ylab = Ylab, xlab = Xlab, ylim = Ylim, xlim=Xlim,
     yaxs = "i",xaxt="n") 
polygon(x=c(3.571429,3.571429,4.941176,4.941176),
        y=c(Ylim[1]+dim.factor,Ylim[2]-dim.factor,Ylim[2]-dim.factor,Ylim[1]+dim.factor),
        col=gray(gray.scale),border=gray(gray.scale),lty=1, bg=TRUE)
polygon(x=c( 5.466667, 5.466667,5.733333,5.733333),
        y=c(Ylim[1]+dim.factor,Ylim[2]-dim.factor,Ylim[2]-dim.factor,Ylim[1]+dim.factor),
        col=gray(gray.scale),border=gray(gray.scale),lty=1, bg=TRUE) 
lines(X_equalspace, Y1*100, lwd = 1.5)
for (i in c(1:16)) {
  lines(X_equalspace, Y0[i,]*100, col = gray(.55), lwd = 1)
}
points(X_equalspace, Y1*100, pch=19, cex = 4/7 )

axis(1,las=1,labels=c(seq(1900,1970,10)),at=X_equal_10years[1:8])

legend(c("topright"), legend = Legend, lty = 1:1, col = c("black", 
                                                          gray(.55)), lwd = c(1.75, 1.75), cex = 6/7)


axis(1,las=1,labels=c(seq(1900,1970,10)),at=X_equal_10years[1:8])