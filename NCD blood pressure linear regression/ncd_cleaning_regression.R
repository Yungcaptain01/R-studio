#### Simulated Nigeria NCD data cleaning and multiple linear regression analysis ####

## LOad necessary packages

library(readxl)
library(tidyverse)
library(writexl)
library(janitor)
library(broom)
library(car)
library(gt)

## Importing ncd excel dataset 

nigeria_ncd_dirty_data <- read_excel("nigeria_ncd_dirty_data.xlsx", skip = 2)

## standardizing column names, removing fully blank and duplicate rows

ncd_data <- nigeria_ncd_dirty_data %>% remove_empty("rows") %>% distinct() %>% clean_names()

## Checking incosistent categorical coding

tabyl(ncd_data, zone)
tabyl(ncd_data, state)
tabyl(ncd_data, sex)
tabyl(ncd_data, smoking_status)
tabyl(ncd_data, physical_activity_level)
tabyl(ncd_data, alcohol_use)
tabyl(ncd_data, family_history_ncd)
tabyl(ncd_data, education_level)

## Standardizing categorical text variables

clean_yesno <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x %in% c("y", "yes") ~ "Yes",
    x %in% c("n", "no")  ~ "No",
    TRUE ~ NA_character_
  )
}

ncd_data <- ncd_data %>%
  mutate(
    sex = str_to_lower(str_trim(as.character(sex))),
    sex = case_when(
      sex %in% c("m", "male")   ~ "Male",
      sex %in% c("f", "female") ~ "Female",
      TRUE ~ NA_character_
    ),
    smoking_status      = clean_yesno(smoking_status),
    alcohol_use         = clean_yesno(alcohol_use),
    family_history_ncd  = clean_yesno(family_history_ncd),
    physical_activity_level = str_to_lower(str_trim(as.character(physical_activity_level))),
    physical_activity_level = case_when(
      physical_activity_level %in% c("low", "sedentary")        ~ "Low",
      physical_activity_level %in% c("moderate", "mod")         ~ "Moderate",
      physical_activity_level %in% c("high", "active")          ~ "High",
      TRUE ~ NA_character_
    ),
    state = str_to_title(str_trim(as.character(state))),
    education_level = str_trim(as.character(education_level)),
    education_level = na_if(education_level, "Unknown"),
    education_level = na_if(education_level, ""),
    education_level = na_if(education_level, "nan")
  )

## Clean numeric fields stored as messy text 

clean_number <- function(x) {
  x <- str_trim(as.character(x))
  x <- na_if(str_to_lower(x), "n/a")
  x <- na_if(x, "nan")
  x <- na_if(x, "missing")
  x <- na_if(x, "")
  suppressWarnings(as.numeric(x))
}

ncd_data <- ncd_data %>%
  mutate(
    monthly_income_naira = str_trim(as.character(monthly_income_naira)),
    monthly_income_naira = na_if(str_to_lower(monthly_income_naira), "refused"),
    monthly_income_naira = str_remove_all(monthly_income_naira, "[^0-9.]"),
    monthly_income_naira = na_if(monthly_income_naira, "nan"),
    monthly_income_naira = suppressWarnings(as.numeric(monthly_income_naira)),

    age                          = clean_number(age),
    height_cm                    = clean_number(height_cm),
    weight_kg                    = clean_number(weight_kg),
    bmi                          = clean_number(bmi),
    systolic_bp                  = clean_number(systolic_bp),
    diastolic_bp                 = clean_number(diastolic_bp),
    fasting_blood_glucose_mmol_l  = clean_number(fasting_blood_glucose_mmol_l),
    total_cholesterol_mmol_l      = clean_number(total_cholesterol_mmol_l)
  )

## Formatting survey date column

ncd_data <- ncd_data %>% mutate(survey_date = 
                                  parse_date_time(survey_date, orders = 
                                                    c("Y-m-d", "m/d/Y", "d-m-Y", "d B Y", "d.m.y")))

## Marking outliers as NA

ncd_data <- ncd_data %>%
  mutate(
    age                          = ifelse(age < 18 | age > 100, NA, age),
    systolic_bp                  = ifelse(systolic_bp < 70 | systolic_bp > 260, NA, systolic_bp),
    diastolic_bp                 = ifelse(diastolic_bp < 40 | diastolic_bp > 150, NA, diastolic_bp),
    height_cm                    = ifelse(height_cm < 120 | height_cm > 210, NA, height_cm),
    weight_kg                    = ifelse(weight_kg < 30 | weight_kg > 250, NA, weight_kg),
    bmi                          = ifelse(bmi < 10 | bmi > 70, NA, bmi),
    fasting_blood_glucose_mmol_l  = ifelse(fasting_blood_glucose_mmol_l < 2 |
                                             fasting_blood_glucose_mmol_l > 20, NA,
                                           fasting_blood_glucose_mmol_l)
  )

## calculating BMI from height & weight where BMI is missing but components are available

ncd_data <- ncd_data %>%
  mutate(
    bmi = ifelse(is.na(bmi) & !is.na(height_cm) & !is.na(weight_kg),
                  weight_kg / (height_cm / 100)^2,
                  bmi)
  )

## Extracting variables needed for linear regression analysis

model_vars <- c("systolic_bp", "age", "sex", "bmi",
                 "fasting_blood_glucose_mmol_l", "total_cholesterol_mmol_l",
                 "smoking_status", "physical_activity_level",
                 "family_history_ncd", "monthly_income_naira")

model_df <- ncd_data %>%
  select(all_of(model_vars)) %>% drop_na() %>%
  mutate(sex = as.factor(sex), 
         smoking_status = as.factor(smoking_status), 
         physical_activity_level = as.factor(physical_activity_level), 
         family_history_ncd = as.factor(family_history_ncd), 
         log_income = log(monthly_income_naira)
  )

## writing clean data into excel

getwd()

write_xlsx(model_df, "C:/Users/USER/OneDrive/Desktop/R studio/NCD blood pressure linear regression/clean_ncd_data.xlsx")

#### Multiple Linear Regression ####

mlr <- lm(
  systolic_bp ~ age + sex + bmi + fasting_blood_glucose_mmol_l +
    total_cholesterol_mmol_l + smoking_status + physical_activity_level +
    family_history_ncd + log_income,
  data = model_df
)

print(summary(mlr))

## Tidy coefficient table (with 95% CIs)

tidy_mlr <- tidy(mlr, conf.int = TRUE)
print(tidy_mlr, n = Inf)

coeff_table <- data.frame(tidy_mlr)
coeff_table %>% gt() %>% gtsave("coeff_table.png")

## Overall fit statistics

glance_mlr <- glance(mlr)
print(glance_mlr)

## checking for multicollinearity among predictor variables

print(vif(mlr))

## Residual diagnostic plots

plot(mlr)

###### Visualizing a couple of key relationships ####

# BMI visualization

ggplot(model_df, aes(x = bmi, y = systolic_bp)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Systolic BP vs BMI", x = "BMI (kg/m^2)", y = "Systolic BP (mmHg)") +
  theme_minimal()

# Physical activity visualization

ggplot(model_df, aes(x = physical_activity_level, y = systolic_bp,
                            fill = physical_activity_level)) + geom_boxplot() +
  labs(title = "Systolic BP by Physical Activity Level",
       x = "Physical Activity Level", y = "Systolic BP (mmHg)") +
  theme_minimal() +
  theme(legend.position = "none")

# Smoking status visualization

ggplot(model_df, aes(smoking_status, systolic_bp, fill = smoking_status)) + 
  geom_boxplot() + labs(title = "Systolic BP by Smoking status", 
                        x = "Smoking Status", 
                        y = "Systolic BP (mmHg)") + 
  theme_minimal() + theme(legend.position = "none")

# Fasting blood glucose visualization

ggplot(model_df, aes(x = fasting_blood_glucose_mmol_l, y = systolic_bp)) +
  geom_point(alpha = 0.4, color = "navyblue") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Systolic BP vs FBS", x = "FBS (mmol/L)", y = "Systolic BP (mmHg)") +
  theme_minimal()

# Family history visualization

ggplot(model_df, aes(family_history_ncd, systolic_bp, fill = family_history_ncd)) + 
  geom_boxplot() + labs(title = "Systolic BP by Family History", 
                        x = "Family History", 
                        y = "Systolic BP (mmHg)") + 
  theme_minimal() + theme(legend.position = "none")
