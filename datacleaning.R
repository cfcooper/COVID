

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(reshape)

rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

fulldat <- read.csv("3.0.csv")
valuesdat <- subset(fulldat, select=c(1,142:157))
demodat <- subset(fulldat, select=c(1:8,158:180))

valuesdat <- merge(valuesdat, demodat)

mean(valuesdat$Q21_1)
mean(valuesdat$Q21_2)
mean(valuesdat$Q21_12)
mean(valuesdat$Q21_13)
mean(valuesdat$Q21_14)
mean(valuesdat$Q21_15)
mean(valuesdat$Q21_16)
mean(valuesdat$Q21_17)

## create consumer impact score ----------------------------------------------------------------

valuesdat$consumerimpact <- (valuesdat$Q242_1 + valuesdat$Q242_2 + valuesdat$Q242_3 + valuesdat$Q242_4 + valuesdat$Q242_6)
summary(valuesdat)

valuesreg <- lm(value ~ Q13 + Q19_1 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = impossible)
summary(impossiblereg)
