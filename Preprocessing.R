rm(list=ls())

library(ggplot2)
library(dplyr)
library(caret)

#Change working directory if needed, Josse.
setwd("C:/Users/Daan/Documents/Projecten/DataMiningResearch")

bfrss.df <- read.csv("BRFSS2015.csv", header = T)


