

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(reshape)
library(corrplot)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

fulldat <- read.csv("3.0.csv")
age_data <- read.csv("age.csv")
income_data <- read.csv("income.csv")
#correct_data <- read.csv("COVID 3.0 numeric 3.15.csv")

#filtered_data <- fulldat[!(fulldat$responseId %in% correct_data$responseId), ]

valuesdat <- subset(fulldat, select=c(1:8,142:157,164))
demodat <- subset(fulldat, select=c(1:8,158:180))

valuesdat <- merge(valuesdat, demodat)

valuesdat <- merge(valuesdat, age_data, by = "age", all.x = TRUE)
valuesdat <- valuesdat[, !names(valuesdat) %in% c("age")]
names(valuesdat)[names(valuesdat) == "age_num"] <- "age"

valuesdat <- merge(valuesdat, income_data, by = "income_2023", all.x = TRUE)


valuesdat$comma <- str_count(valuesdat$fv_top_three_factors, ",")
valuesdat <- valuesdat[valuesdat$comma == 2, ]



## factors corr -----------------------------------------------------------------------------

value_corr <- subset(valuesdat, select=c(2,25))

value_corr$local <- if_else(str_detect(value_corr$fv_top_three_factors,"1"), 1, 0)
value_corr$organic <- if_else(str_detect(value_corr$fv_top_three_factors,"2"), 1, 0)
value_corr$local_econ <- if_else(str_detect(value_corr$fv_top_three_factors,"3"), 1, 0)
value_corr$affordable <- if_else(str_detect(value_corr$fv_top_three_factors,"4"), 1, 0)
value_corr$health <- if_else(str_detect(value_corr$fv_top_three_factors,"5"), 1, 0)
value_corr$socialresp <- if_else(str_detect(value_corr$fv_top_three_factors,"6"), 1, 0)
value_corr$convenient <- if_else(str_detect(value_corr$fv_top_three_factors,"7"), 1, 0)

value_corr2 <- subset(value_corr, select=c(3:9))
correlation_matrix <- cor(value_corr2)
print(correlation_matrix)

corrplot(correlation_matrix, method="circle")
correlation_df <- as.data.frame(correlation_matrix)






## top factors total -----------------------------------------------------------------------------

valuesdat$comma <- str_count(valuesdat$fv_top_three_factors, ",")
topfactors <- valuesdat[!valuesdat$comma > 2, ]