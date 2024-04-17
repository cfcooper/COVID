

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(reshape)
library(haven)
library(broom)
library(MASS)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

fulldat <- read_stata("COVID 3.0 JR 4_15_24.dta")

col_names <- colnames(fulldat)
col_positions <- seq_along(col_names)

# Combine column names and positions
col_info <- data.frame(Column = col_names, Position = col_positions)


local_expend <- subset(fulldat, select = c(1:184,438:466,628,633:634))
local_expend$loyallocal <- ifelse(local_expend$localpct2023 > (.0695698 + .1379803), 1, 0)
local_expend <- local_expend[!is.na(local_expend$loyallocal), ]

loyallocal_pce <- local_expend %>% group_by(loyallocal) %>%
  summarise(mean_pcelocal = mean(pce_localecon),
            mean_pcesociety = mean(pce_society))

write.csv(loyallocal_pce, "loyallocal.csv")

## adjusted regional motivations ------------------------------------------------------------
col_names <- colnames(local_expend)
col_positions <- seq_along(col_names)

# Combine column names and positions
col_info <- data.frame(Column = col_names, Position = col_positions)



region_factor <- subset(local_expend, select = c(1:6,155:178,185:198,214:217))

region_factors <- region_factor %>% group_by(rucc) %>%
  summarise(mean_pcelocal = mean(pce_localecon),
            mean_pcesociety = mean(pce_society))




region_factor$comma <- str_count(region_factor$fv_top_three_factors, ",")
topfactors <- region_factor[!region_factor$comma > 2, ]

factors <- topfactors %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
factors$adjust <- (factors$count/3757)*100

density <- topfactors %>% group_by(rucc) %>%
  summarise(sum = n())


topfactors %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")


factors_overall <- topfactors %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
factors_overall$adjust <- (factors_overall$count/3757)*100


values <- read.csv("values.csv")
factors_overall <- merge(factors_overall, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_overall <- factors_overall[, !names(factors_overall) %in% c("fv_top_three_factors")]
write.csv(factors_overall, "purchasefactors.csv")



factors_density <- topfactors %>% group_by(rucc,fv_top_three_factors) %>%
  summarise(count = n())


factors_density <- left_join(factors_density, density, by = "rucc")
factors_density$adjust <- (factors_density$count/factors_density$sum)*100
factors_density <- merge(factors_density, values, by.x = "fv_top_three_factors", by.y = "fv_code", all.x = TRUE)
factors_density <- factors_density[, !names(factors_density) %in% c("fv_top_three_factors")]



factors_density$density <- if_else(factors_density$rucc %in% c("1"), "Urban", "x")
factors_density$density <- if_else(factors_density$rucc %in% c("2"), "Suburban", factors_density$density)
factors_density$density <- if_else(factors_density$rucc %in% c("3"), "Rural", factors_density$density)



ggplot(factors_density, aes(fill=value, y=adjust, x=density)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = paste0(round(adjust,1), "%"), 
                y = adjust + 1.1),  # Adjust y-coordinate for label position
            position = position_dodge(width = 0.9), 
            color = "black", size = 3) +
  labs(title = "Top 3 Motivations for Food Purchases", x = "", y = "") +  # Change axis labels
  theme_minimal() +
  theme(text = element_text(size = 14))

write.csv(factors_density, "density_factors.csv")

## expenditure percentages -------------------------------------------------------------------------------------

fulldat

fulldat$comma <- str_count(fulldat$fv_top_three_factors, ",")
topfactors <- fulldat[!fulldat$comma > 2, ]

col_names <- colnames(topfactors)
col_positions <- seq_along(col_names)

# Combine column names and positions
col_info <- data.frame(Column = col_names, Position = col_positions)


expend_percent <- subset(topfactors, select = c(1:6,155:169,177:184,278:291,306:309,438:451,609,624:625,628:629,631,633,635:648))
expend_percent <- expend_percent[!is.na(expend_percent$localpct2023), ]


col_names <- colnames(expend_percent)
col_positions <- seq_along(col_names)

# Combine column names and positions
col_info <- data.frame(Column = col_names, Position = col_positions)

expend_percent2 <- subset(expend_percent, select = c(63:82))

#expend_percent <- expend_percent[!expend_percent$totalexp_FAH_oct23 < 5, ]
expend_percent <- expend_percent[!expend_percent$total_exp_2023 < 50, ]
expend_percent <- expend_percent[!is.na(expend_percent$localpct2023), ]
expend_percent$local <- (expend_percent$localpct2023)*100

expend_percent$topfact_afford <- if_else(str_detect(expend_percent$fv_top_three_factors,"4"), 1, 0)
expend_percent$topfact_locallygrown <- if_else(str_detect(expend_percent$fv_top_three_factors,"1"), 1, 0)
expend_percent$topfact_organicgrown <- if_else(str_detect(expend_percent$fv_top_three_factors,"2"), 1, 0)
expend_percent$topfact_localecon <- if_else(str_detect(expend_percent$fv_top_three_factors,"3"), 1, 0)
expend_percent$topfact_health <- if_else(str_detect(expend_percent$fv_top_three_factors,"5"), 1, 0)
expend_percent$topfact_society <- if_else(str_detect(expend_percent$fv_top_three_factors,"6"), 1, 0)
expend_percent$topfact_convenient <- if_else(str_detect(expend_percent$fv_top_three_factors,"7"), 1, 0)



localspend_reg <- lm(localpct2023 ~ rucs + income_2023 + educ + female + gender_other + children +
                       topfact_afford + topfact_locallygrown + topfact_organicgrown + topfact_localecon + topfact_health +
                       topfact_society + topfact_convenient + pce_localecon + pce_society, 
                      data = expend_percent)
summary(localspend_reg)
regression_results <- tidy(localspend_reg)
summary(regression_results)


  
loyal_local_reg <- lm(localpct2023 ~ rucs + income_2023 + educ + female + gender_other + poverty + children + fv_locallygrown + fv_organicgrown +
                          fv_localecon + fv_afford + fv_workers + pce_localecon + pce_society , 
                        data = expend_percent)
summary(loyal_local_reg)


glm_local <- glm(localpct2023 ~ rucs + income_2023 + educ + female + gender_other + children +
               topfact_afford + topfact_locallygrown + topfact_organicgrown + topfact_localecon + topfact_health +
               topfact_society + topfact_convenient + pce_localecon + pce_society, 
             family = betaprobit(link = "logit"), 
             data = expend_percent)
summary(glm_local)











## PCE regressions

pce_local_reg <- lm(pce_localecon ~ rucs + income_2023 + educ + female + gender_other + children +
                      topfact_afford + topfact_locallygrown + topfact_organicgrown + topfact_localecon + topfact_health +
                      topfact_society + topfact_convenient, 
                      data = expend_percent)
summary(pce_local_reg)
























