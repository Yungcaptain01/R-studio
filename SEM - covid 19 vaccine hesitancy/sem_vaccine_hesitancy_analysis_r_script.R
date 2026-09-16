## STRUCTURAL EQUATION MODELING (SEM): VACCINE HESITANCY & COVID-19 VACCINATION UPTAKE ####

## Loading necessary packages

library(lavaan)         # SEM / CFA engine
library(readxl)         # read the excel dataset
library(tidyverse)      # data wrangling
library(semPlot)        # path diagrams
library(gt)             # table to image

## Loading vaccine hesitancy data set

vaccine_hesitancy_rough_data <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/SEM - covid 19 vaccine hesitancy/vaccine_hesitancy_rough_data.xlsx")


## Specifying measurement model (Confirmatory Factor Analysis)

measurement_model <- '
  Hesitancy     =~ h1_distrust_govt + h2_safety_concern + h3_conspiracy_belief + h4_natural_immunity
  PerceivedRisk =~ r1_infection_risk + r2_severity_risk + r3_community_risk
  SocialNorms   =~ sn1_family_support + sn2_provider_trust + sn3_peer_norms
  Uptake        =~ u1_intention + u2_timeliness
'

## Fitting the CFA to measurement model 

cfa_fit <- cfa(measurement_model, data = vaccine_hesitancy_rough_data, estimator = "MLR")
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)

## Key fit indices to report

fitMeasures(cfa_fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                        "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))


## Specifying the structural equation model (SEM)

structural_model <- '
  # Measurement model
  Hesitancy     =~ h1_distrust_govt + h2_safety_concern + h3_conspiracy_belief + h4_natural_immunity
  PerceivedRisk =~ r1_infection_risk + r2_severity_risk + r3_community_risk
  SocialNorms   =~ sn1_family_support + sn2_provider_trust + sn3_peer_norms
  Uptake        =~ u1_intention + u2_timeliness

  # Structural (regression) paths, each coefficient given a label 
  Hesitancy     ~ b1*SocialNorms
  PerceivedRisk ~ b2*Hesitancy + b3*SocialNorms
  Uptake        ~ c_direct*Hesitancy + b4*PerceivedRisk + b5*SocialNorms

  # Defined parameters: indirect / total effects 
  indirect_via_risk       := b2 * b4
  total_hesitancy_effect  := c_direct + indirect_via_risk
'

sem_fit <- sem(structural_model, data = vaccine_hesitancy_rough_data, estimator = "MLR")
summary(sem_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## Key fit overall indices to report

fit_indices <- fitMeasures(sem_fit, c("chisq", "df", "pvalue", "cfi", "tli",
                                       "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
print(round(fit_indices, 3))

cat("\nRule-of-thumb interpretation:\n",
    "- CFI/TLI > 0.90 (good: > 0.95)\n",
    "- RMSEA  < 0.08 (good: < 0.06)\n",
    "- SRMR   < 0.08\n")


## Inspection of standardized structural paths

standardized_paths <- standardizedSolution(sem_fit) %>%
  filter(op %in% c("~", ":=")) %>%
  select(lhs, op, rhs, est.std, se, pvalue, ci.lower, ci.upper)

print(standardized_paths, digits = 3)

cat("\nHow to read this:\n",
    "- Uptake ~ Hesitancy (c_direct): the DIRECT effect of vaccine hesitancy on\n",
    "  vaccination uptake, controlling for perceived risk and social norms.\n",
    "  A negative, significant coefficient supports the hypothesis that more\n",
    "  hesitant individuals are less likely to be vaccinated.\n",
    "- indirect_via_risk: the INDIRECT effect of hesitancy on uptake that runs\n",
    "  through reduced perceived risk (hesitant people underestimate their risk,\n",
    "  which further lowers uptake).\n",
    "- total_hesitancy_effect: direct + indirect effect combined.\n")

## Visualizing Path diagram

semPaths(sem_fit,
         what = "std",
         whatLabels = "std",
         layout = "tree2",
         edge.label.cex = 1.2,
         sizeMan = 6,
         sizeLat = 18,
         residuals = FALSE,
         nCharNodes = 0,
         title = FALSE,
         curvePivot = TRUE)
title("SEM: Vaccine Hesitancy and COVID-19 Vaccination Uptake\n(standardized coefficients)",
      cex.main = 0.9)

## Tidying standardized path and exporting results table to image

results_table <- standardized_paths %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

results_table %>% gt() %>% gtsave("results_table.png")


