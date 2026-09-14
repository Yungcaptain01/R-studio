
## Loading necessary packages

library(readxl)      # reading excel file into R
library(writexl)     # writing cleaned data into Excel
library(dplyr)       # data cleaning
library(janitor)     # clean_names(), tabyl()
library(forcats)     # factor releveling
library(ggplot2)     # Data visualization
library(broom)       # tidy() model output
library(car)         # vif() for multicollinearity
library(stringr)     # standardize text variables
library(gt)          # Save coefficient table

## Import rough ncd data

rough_ngn_ncd_data <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/Nigeria NCD risk factor/Nigeria NCD risk factor and regression analysis/rough-nigeria-ncd-survey-data.xlsx")

###### Data Cleaning ####

## standardizing variable names

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% clean_names()

## Inspection of duplicate rows

sum(duplicated(rough_ngn_ncd_data$respondent_id))
sum(duplicated(rough_ngn_ncd_data))

## Removing duplicate rows

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% distinct(respondent_id, .keep_all = TRUE)

## Dropping fully blank rows

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% filter(if_any(everything(), ~ !is.na(.)))

## Looking at inconsistent categorical coding

tabyl(rough_ngn_ncd_data, sex)
tabyl(rough_ngn_ncd_data, residence)
tabyl(rough_ngn_ncd_data, education)
tabyl(rough_ngn_ncd_data, smoking_status)

## Standardizing sex to a two-level factor

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% mutate(sex = str_to_lower(sex), 
                                                    sex = case_when(sex %in% c("m", "male") ~ "Male", 
                                                                    sex %in% c("f", "female") ~ "Female", 
                                                                    TRUE ~ NA_character_))

## Removing implausible age values

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% mutate(age = ifelse(age < 18 | age > 99, NA_real_, age))

## Removing implausible systolic BP (physiologically plausible range ~ 70-260 mmHg)

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% mutate(systolic_bp = ifelse(systolic_bp < 70 | systolic_bp > 260, NA_real_, systolic_bp))

## Converting categorical variables into factors

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% 
  mutate(residence = as_factor(residence), 
         education = as_factor(education), 
         smoking_status = as_factor(smoking_status), 
         geopolitical_zone = as_factor(geopolitical_zone), 
         state = as_factor(state), 
         sex = as_factor(sex))

## Deriving BMI category according to WHO standard

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% mutate(bmi_category = as.factor(case_when(
  is.na(bmi) ~ NA_character_, 
  bmi < 18.5 ~ "Underweight", 
  bmi < 25 ~ "Normal", 
  bmi < 30 ~ "Overweight", 
  TRUE ~ "Obese"
)))

## Recoding binary indicators as labeled factors

rough_ngn_ncd_data <- rough_ngn_ncd_data %>% mutate(
    current_smoker         = factor(current_smoker, levels = c(0, 1), labels = c("No", "Yes")),
    alcohol_current_use    = factor(alcohol_current_use, levels = c(0, 1), labels = c("No", "Yes")),
    low_physical_activity  = factor(low_physical_activity, levels = c(0, 1), labels = c("No", "Yes")),
    family_history_ncd     = factor(family_history_ncd, levels = c(0, 1), labels = c("No", "Yes")),
    hypertension_status    = factor(hypertension_status, levels = c(0, 1), labels = c("No", "Yes"))
  )

## Writing cleaned data into excel

Cleaned_ngn_ncd_survey_data <- rough_ngn_ncd_data

write_xlsx(Cleaned_ngn_ncd_survey_data, "C:/Users/USER/OneDrive/Documents/Cleaned_ngn_ncd_survey_data.xlsx")

## Selecting variable needed for logistic regression analysis

model_vars <- c("hypertension_status", "age", "sex", "residence", "bmi",
                 "smoking_status", "low_physical_activity",
                 "fasting_blood_glucose_mmol_l", "family_history_ncd")

model_data <- rough_ngn_ncd_data %>%
  select(all_of(model_vars)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

## Exploratory summary (EDA)

summary(model_data)

tabyl(model_data, hypertension_status, sex) %>% adorn_percentages("all") %>% adorn_pct_formatting()

ggplot(rough_ngn_ncd_data, aes(x = hypertension_status, y = bmi, fill = hypertension_status)) +
  geom_boxplot() +
  labs(title = "BMI by Hypertension Status", x = "Hypertension", y = "BMI (kg/m^2)") +
  theme_minimal()

ggplot(rough_ngn_ncd_data, aes(x = hypertension_status, y = fasting_blood_glucose_mmol_l, fill = hypertension_status)) +
  geom_boxplot() +
  labs(title = "Blood glucose by Hypertension Status", x = "Hypertension", y = "Blood glucose (mmol/L)") +
  theme_minimal()

##### Logistic regression analysis ####

# Reference levels

model_data <- model_data %>%
  mutate(
    hypertension_status = fct_relevel(hypertension_status, "No"),
    sex = fct_relevel(sex, "Male"),
    residence = fct_relevel(residence, "Rural"),
    smoking_status = fct_relevel(smoking_status, "Never smoked"),
    low_physical_activity = fct_relevel(low_physical_activity, "No"),
    family_history_ncd = fct_relevel(family_history_ncd, "No")
  )

logit_model <- glm(
  hypertension_status ~ age + sex + residence + bmi + smoking_status +
    low_physical_activity + fasting_blood_glucose_mmol_l,
  data = model_data,
  family = binomial(link = "logit")
)

logit_sum <- tidy(logit_model)

## Tidy coefficient table with odds ratios and 95% CIs

OR_table <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

print(OR_table)

OR_table %>% gt() %>% gtsave("OR_table.png")

## Multicollinearity check: To see if the predictors are correlated in values or not

vif(logit_model)

VIF_table <- data.frame(vif(logit_model))

VIF_table %>% gt() %>% gtsave("VIF_table.png")
