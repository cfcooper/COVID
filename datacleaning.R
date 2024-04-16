

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




## top factors total -----------------------------------------------------------------------------

valuesdat$comma <- str_count(valuesdat$fv_top_three_factors, ",")
topfactors <- valuesdat[!valuesdat$comma > 2, ]

factors <- topfactors %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
factors$adjust <- (factors$count/4263)*100

factors_d <- topfactors %>% group_by(Q101, fv_top_three_factors) %>%
  summarise(count = n())

factors_d <- factors_d[!factors_d$Q101 == "4", ]
factors_d <- na.omit(factors_d)

density <- topfactors %>% group_by(Q101) %>%
  summarise(sum = n())
density <- na.omit(density)
factors_d <- left_join(factors_d, density, by = "Q101")
factors_d$adjust <- (factors_d$count/factors_d$sum)*100




## top factors grouped -----------------------------------------------------------------------------



topfactors %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")

factors <- valuesdat %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
factors$adjust <- (factors$count/4263)*100

factors_overall <- topfactors %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
factors_overall$adjust <- (factors_overall$count/4263)*100


values <- read.csv("values.csv")
factors_overall <- merge(factors_overall, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_overall <- factors_overall[, !names(factors_overall) %in% c("fv_top_three_factors")]
write.csv(factors_overall, "purchasefactors.csv")

income <- topfactors %>% group_by(income_2023) %>%
  summarise(sum = n())
factors_income <- topfactors %>% group_by(income_2023,fv_top_three_factors) %>%
  summarise(count = n())
values <- read.csv("values.csv")
factors_income <- merge(factors_income, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_income <- factors_income[, !names(factors_income) %in% c("fv_top_three_factors")]

factors_age <- topfactors %>% group_by(age,fv_top_three_factors) %>%
  summarise(count = n())
age <- topfactors %>% group_by(age) %>%
  summarise(sum = n())

factors_income <- left_join(factors_income, income, by = "income_2023")
factors_income$adjust <- (factors_income$count/factors_income$sum)*100

factors_age <- left_join(factors_age, age, by = "age")
factors_age$adjust <- (factors_age$count/factors_age$sum)*100
factors_age <- merge(factors_age, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_age <- factors_age[, !names(factors_age) %in% c("fv_top_three_factors")]


factors_density <- topfactors %>% group_by(Q101,fv_top_three_factors) %>%
  summarise(count = n())
##density <- topfactors %>% group_by(Q101) %>%
##  summarise(sum = n())

factors_density <- left_join(factors_density, density, by = "Q101")
factors_density$adjust <- (factors_density$count/factors_density$sum)*100
factors_density <- merge(factors_density, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_density <- factors_density[, !names(factors_density) %in% c("fv_top_three_factors")]
factors_density <- factors_density[!factors_density$Q101 == "4", ]
factors_density <- na.omit(factors_density)
factors_density$Q101 <- as.character(factors_density$Q101)
factors_density$Q101 <- if_else(factors_density$Q101 %in% c("1"), "Rural", factors_density$Q101)
factors_density$Q101 <- if_else(factors_density$Q101 %in% c("2"), "Suburban", factors_density$Q101)
factors_density$Q101 <- if_else(factors_density$Q101 %in% c("3"), "Urban", factors_density$Q101)

valuesdat$density <- if_else(valuesdat$Q101 %in% c("1"), "Rural", "x")
valuesdat$density <- if_else(valuesdat$Q101 %in% c("2"), "Suburban", valuesdat$density)
valuesdat$density <- if_else(valuesdat$Q101 %in% c("3"), "Urban", valuesdat$density)

ggplot(factors_income, aes(fill=value, y=adjust, x=income_2023)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(factors_age, aes(fill=value, y=adjust, x=age)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(factors_density, aes(fill=value, y=adjust, x=Q101)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = paste0(round(adjust,1), "%"), 
            y = adjust + 1.1),  # Adjust y-coordinate for label position
position = position_dodge(width = 0.9), 
color = "black", size = 3) +
  labs(title = "Top 3 Motivations for Food Purchases", x = "", y = "") +  # Change axis labels
  theme_minimal() +
  theme(text = element_text(size = 14))

write.csv(factors_density, "density_factors.csv")



## add regions to data ------------------------------------------------------------------------

region <- read.csv("state_region.csv")

valuesdat <- left_join(valuesdat, region, by = c("state"))

regionlocal <- valuesdat %>% group_by(region) %>%
  summarise(meanval = mean(Q21_12))

regionworkers <- valuesdat %>% group_by(region) %>%
  summarise(meanval = mean(Q21_16))


## top factor reg ----------------------------------------------------------------------------------

valuesdat$topvalue <- if_else(valuesdat$fv_top_three_factors %in% c("4,5,7"), 1, 0)
valuesdat <- valuesdat[!valuesdat$age == "Under 18", ]

valuesdat$children_bi <- if_else(valuesdat$children %in% c("0"), 0, 1)

valuesdat <- valuesdat[!valuesdat$Q101 == "4", ]


model <- lm(topvalue ~ density + children_bi + age + poverty + income + region, data = valuesdat, family = binomial)
summary(model)




## percieved consumer effectiveness scale -----------------------------------------------------------------


valuesdat$consumerimpact <- (valuesdat$Q242_1 + valuesdat$Q242_2 + valuesdat$Q242_3 + valuesdat$Q242_4 + valuesdat$Q242_6)

median(valuesdat$consumerimpact)

valuesdat$lowPCE <- if_else(valuesdat$consumerimpact > 20, 0, 1)
valuesdat$highPCE <- if_else(valuesdat$lowPCE > .5, 0, 1)

PCE <- valuesdat[, c("responseId", "consumerimpact","highPCE")]

highlowPCE <- valuesdat %>% group_by(poverty,highPCE) %>%
  summarise(count = n())

incomePCE <- valuesdat %>% group_by(income_2023) %>%
  summarise(meanval = mean(consumerimpact))

regionPCE <- valuesdat %>% group_by(region) %>%
  summarise(meanval = mean(consumerimpact))

topfactors <- merge(topfactors, PCE, by = "responseId")

factors_PCE <- topfactors %>% group_by(highPCE,fv_top_three_factors) %>%
  summarise(count = n())
factors_PCE2 <- topfactors %>% group_by(highPCE) %>%
  summarise(sum = n())
factors_PCE <- left_join(factors_PCE, factors_PCE2, by = "highPCE")
factors_PCE$adjust <- (factors_PCE$count/factors_PCE$sum)*100

factors_PCE <- merge(factors_PCE, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_PCE <- factors_PCE[, !names(factors_PCE) %in% c("fv_top_three_factors")]

factors_PCE$PCE <- if_else(factors_PCE$highPCE > .5, "high", "low")

values_binary <- fulldat[, c("responseId", "incr_mktshopping_values","incr_brandshopping_values")]


mktshop_values <- values_binary %>% group_by(incr_mktshopping_values) %>%
  summarise(count = n())
mktshop_values$adjust <- (mktshop_values$count/4562)*100



brandshop_values <- values_binary %>% group_by(incr_brandshopping_values) %>%
  summarise(count = n())
brandshop_values$adjust <- (brandshop_values$count/4562)*100



ggplot(factors_PCE, aes(fill=value, y=adjust, x=PCE)) + 
  geom_bar(position="dodge", stat="identity")

write.csv(factors_PCE, "PCE_factors.csv")

## create consumer impact score ----------------------------------------------------------------


PCE <- lm(consumerimpact ~ density + children_bi + Q5 + poverty + income_2023 + region + educ + Q50, data = valuesdat, family = multinomial)
summary(PCE)




objects <- ls()
datasets_to_keep <- c("fulldat", "topfactors","valuesdat")
objects_to_remove <- setdiff(objects, datasets_to_keep)
rm(list = objects_to_remove)  # Caution: this clears the Environment


## analysis of value scores ---------------------------------------------------------

valuesdat$test <- 1
valuesdat <- na.omit(valuesdat)

summary(valuesdat)

mean(valuesdat$Q21_1)   # local
mean(valuesdat$Q21_2)   # organic
mean(valuesdat$Q21_12)  # supports local econ
mean(valuesdat$Q21_13)  # affordable
mean(valuesdat$Q21_14)  # traditional/cultural pref
mean(valuesdat$Q21_16)  # equality
mean(valuesdat$Q21_15)  # food safety
mean(valuesdat$Q22_6)   # worker safety
mean(valuesdat$Q22_8)   # options on purchase method

overall_values <- valuesdat %>%
  summarise(Local = mean(Q21_1),
            Organic = mean(Q21_2),
            Supports_LocalEcon = mean(Q21_12),
            Afforable = mean(Q21_13),
            CulturalPref = mean(Q21_14),
            GenderEqual = mean(Q21_16),
            FoodSafety = mean(Q21_15),
            WorkerSafety = mean(Q22_6),
            PurchaseMethods = mean(Q22_8))
vertical_overall <- pivot_longer(overall_values, cols = c(Local, Organic, Supports_LocalEcon, Afforable,
                                                          CulturalPref, GenderEqual, FoodSafety, WorkerSafety, PurchaseMethods), 
                              names_to = "variable", values_to = "value")
vertical_overall <- vertical_overall[order(vertical_overall$value), ]
ggplot(vertical_overall, aes(x = reorder(variable, value), y = value)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1) + theme_minimal())




income_values <- valuesdat %>% group_by(income) %>%
  summarise(mean_local = mean(Q21_1),
            mean_organic = mean(Q21_2),
            mean_localecon = mean(Q21_12),
            mean_affordable = mean(Q21_13),
            mean_culture = mean(Q21_14),
            mean_gender = mean(Q21_16),
            mean_foodsafe = mean(Q21_15),
            mean_workersafe = mean(Q22_6),
            mean_purchase = mean(Q22_8))


income_values$income = factor(income_values$income, levels = c("Less than $10,000",
                                                               "$10,000 - $19,999",
                                                               "$20,000 - $29,999",
                                                               "$30,000 - $39,99",
                                                               "$40,000 - $49,999",
                                                               "$50,000 - $59,999",
                                                               "$60,000 - $69,999",
                                                               "$70,000 - $79,999",
                                                               "$80,000 - $89,999",
                                                               "$90,000 - $99,999",
                                                               "$100,000 - $149,999",
                                                               "$150,000 or more"))


ggplot(income_values, aes(y=mean_purchase, x=income)) + 
  geom_bar(position="dodge", stat="identity") + scale_y_continuous(limits = c(0, 7))

vertical_data <- pivot_longer(income_values, cols = c(mean_local, mean_organic, mean_localecon, mean_affordable,
                                             mean_culture, mean_gender, mean_foodsafe, mean_workersafe, mean_purchase), 
                              names_to = "variable", values_to = "value")

vertical_data <- na.omit(vertical_data)

ggplot(vertical_data, aes(fill= income, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity")



factors_density <- topfactors %>% group_by(Q101,fv_top_three_factors) %>%
  summarise(count = n())


density_values <- valuesdat %>% group_by(density) %>%
  summarise(mean_local = mean(Q21_1),
            mean_organic = mean(Q21_2),
            mean_localecon = mean(Q21_12),
            mean_affordable = mean(Q21_13),
            mean_culture = mean(Q21_14),
            mean_gender = mean(Q21_16),
            mean_foodsafe = mean(Q21_15),
            mean_workersafe = mean(Q22_6),
            mean_purchase = mean(Q22_8))

density_vertical <- pivot_longer(density_values, cols = c(mean_local, mean_organic, mean_localecon, mean_affordable,
                                                      mean_culture, mean_gender, mean_foodsafe, mean_workersafe, mean_purchase), 
                              names_to = "variable", values_to = "value")

density_vertical <- na.omit(density_vertical)

ggplot(density_vertical, aes(fill= variable, y=value, x=density)) + 
  geom_bar(position="dodge", stat="identity") + scale_y_continuous(limits = c(0, 7))


ggplot(density_values, aes(y=mean_local, x=density)) + 
  geom_bar(position="dodge", stat="identity") + scale_y_continuous(limits = c(0, 7))


ggplot(factors_density, aes(fill=value, y=adjust, x=Q101)) + 
  geom_bar(position="dodge", stat="identity")


## value regression --------------------------------------------------------------------------------


local_reg <- lm(Q21_1 ~ density + male + female + HHsize + Q5 + poverty + income_2023 + educ + Q50, data = valuesdat, family = multinomial)
summary(local_reg)

gender_reg <- lm(Q21_16 ~ density + gender_other + female + HHsize + Q5 + poverty + income_2023 + educ + Q50, data = valuesdat, family = multinomial)
summary(gender_reg)

localecon_reg <- lm(Q21_12 ~ density + gender_other + female + HHsize + Q5 + poverty + income_2023 + educ + Q50, data = valuesdat, family = multinomial)
summary(localecon_reg)

organic_reg <- lm(Q21_2 ~ density + gender_other + female + HHsize + Q5 + poverty + income_2023 + educ + Q50, data = valuesdat, family = multinomial)
summary(organic_reg)





























