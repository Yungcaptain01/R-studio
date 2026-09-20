#### ANCOVA analysis of vaccine survey in 3 U.S states: Alabama, Illinois, and Texas ####

library(tidyverse)
library(readxl)
library(tidyr)
library(car)       # Anova() for Type III sums of squares
library(emmeans)   # adjusted (estimated marginal) means + Tukey contrasts

## Loading csv data file

vaccine_survey_usa <- read.csv("vaccine_survey_usa.csv") 

## converting categorical variables to factors

vaccine_survey_usa <- vaccine_survey_usa %>% mutate(
    vax_status = as.factor(vax_status),
    site       = as.factor(site),
    sex        = as.factor(sex),
    race_eth   = as.factor(race_eth)
  )

## Fiting the ANCOVA model; considering vaccination site interaction on vaccine status: does the effect of vaccination status differ by site

# Perceived risk 

mod_pr <- lm(perceived_risk ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_pr, type = "III")

emmeans(mod_pr, ~ vax_status | site) %>% 
  contrast("pairwise", adjust="tukey")

p <- plot(emmeans(mod_pr, ~ vax_status | site), comparisons = TRUE)

p + labs(
  title = "Perceived risk score", 
  x = "Estimated marginal mean", 
  y = "Vaccination status"
) + theme(plot.title = element_text(hjust = 0.5))

# vaccine Importance

mod_vi <- lm(vaccine_importance ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_vi, type = "III")

emmeans(mod_vi, ~ vax_status | site) %>% 
  contrast("pairwise", adjust = "tukey")

p <- plot(emmeans(mod_vi, ~ vax_status | site), comparisons = TRUE)

p + labs(
  title = "Vaccine importance score", 
  x = "Estimated marginal mean", 
  y = "Vaccination status"
) + theme(plot.title = element_text(hjust = 0.5))

# Social Responsibility

mod_sr <- lm(social_responsibility ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_sr, type = "III")

emmeans(mod_vi, ~ vax_status | site) %>% 
  contrast("pairwise", adjust = "tukey")

p <- plot(emmeans(mod_sr, ~ vax_status | site), comparisons = TRUE)

p + labs(
  title = "Social responsibility score", 
  x = "Estimated marginal mean", 
  y = "Vaccination status"
) + theme(plot.title = element_text(hjust = 0.5))

# Vaccine Safety

mod_vs <- lm(vaccine_safety ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_vs, type = "III")

emmeans(mod_vs, ~ vax_status | site) %>% 
  contrast("pairwise", adjust = "tukey")

p <- plot(emmeans(mod_vs, ~ vax_status | site), comparisons = TRUE)

p + labs(
  title = "Vaccine safety score", 
  x = "Estimated marginal mean", 
  y = "Vaccination status"
) + theme(plot.title = element_text(hjust = 0.5))

# Trust in public health

mod_tph <- lm(trust_ph ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_tph, type = "III")

emmeans(mod_tph, ~ vax_status | site) %>% 
  contrast("pairwise", adjust = "tukey")

plot(emmeans(mod_tph, ~ vax_status | site), comparisons = TRUE)

# Incentives

mod_icen <- lm(incentive ~ vax_status * site + age + sex + race_eth, data = vaccine_survey_usa)

Anova(mod_icen, type = "III")

emmeans(mod_icen, ~ vax_status | site) %>% 
  contrast("pairwise", adjust = "tukey")

plot(emmeans(mod_icen, ~ vax_status | site), comparisons = TRUE)




