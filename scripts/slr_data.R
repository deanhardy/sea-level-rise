# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/data/R/")

rm(list=ls())

library(tidyverse) ## load tidyverse package

## import data from Sweet et al 2017, grid pt 31.5, 278.5 just inland in McIntosh
slr <- read.csv("data/slr/slr.noaa17.csv", header=T)

