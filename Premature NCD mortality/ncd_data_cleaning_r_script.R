#### Data Cleaning ####

## Loading necessary package

library(tidyverse)
library(readxl)
library(janitor)
library(tidyr)
library(broom)
library(writexl)
## Import messy data

messy_ncd_export <- raw_ncd_export <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/who-ncd-portfolio-messy/data/raw/raw_ncd_export.xlsx", sheet = "raw_export")

## Clean variable names

messy_ncd_export <- raw_ncd_export %>% clean_names()

## Drop fully blank rows 

messy_ncd_export <- messy_ncd_export %>% filter(if_any(everything(), ~!is.na(.)))

## Drop duplicate rows

messy_ncd_export <- messy_ncd_export %>% distinct()

## Clean country names

messy_ncd_export <- messy_ncd_export %>% mutate(country = str_to_title(country), country = str_trim(country))

country_names <- c(
  "U.k." = "United Kingdom",
  "United Kingdom Of Great Britain And Northern Ireland" = "United Kingdom",
  "United States Of America" = "United States", 
  "Usa" = "United States",
  "Viet Nam" = "Vietnam", 
  "United Republic Of Tanzania" = "Tanzania", 
  "Korea, Rep." = "South Korea", 
  "Republic Of Korea" = "South Korea"
)

messy_ncd_export <- messy_ncd_export %>% mutate(country = recode(country, !!!country_names))

## Clean region variable

messy_ncd_export <- messy_ncd_export %>% mutate(region = str_trim(str_to_title(region)))

region_names <- c(
  "E. Mediterranean" = "Eastern Mediterranean", 
  "Se Asia" = "South-East Asia"
)

messy_ncd_export <- messy_ncd_export %>% mutate(region = recode(region, !!!region_names))

## Clean sex variable

messy_ncd_export <- messy_ncd_export %>% mutate(sex = str_trim(str_to_title(sex)))

sex_names <- c(
  "B" = "Both Sexes", 
  "Both" = "Both Sexes", 
  "Btsx" = "Both Sexes"
)

messy_ncd_export <- messy_ncd_export %>% mutate(sex = recode(sex, !!!sex_names))

## Clean year variable

messy_ncd_export <- messy_ncd_export %>% mutate(year = case_when(
  year == "'18" ~ 2018, 
  year == "'19" ~ 2019, 
  year == "'20" ~ 2020, 
  year == "'21" ~ 2021, 
  year == "'22" ~ 2022, 
  TRUE ~ as.integer(stringr::str_extract(year, "\\d{4}"))
)) 

## Standardize indicator

messy_ncd_export <- messy_ncd_export %>% mutate(indicator = case_when(
  indicator %in% c("Adult obesity, age-std", "NCD_BMI_30A", "Obesity (%)", "obesity_pct") ~ "obesity_pct", 
  indicator %in% c("Fasting glucose >=7.0 mmol/L", "NCD_GLUC_04", "Raised glucose (%)", "raised_glucose_pct") ~ "raised_glucose_pct", 
  indicator %in% c("NCD Mortality (%)", "NCDMORT3070", "Premature NCD death rate", "ncd_mortality_pct") ~ "ncd_mortality_pct"
))

## Clean value

clean_value <- function(x) {
  x <- na_if(x, "NULL")
  x <- na_if(x, "n/a")
  x <- na_if(x, "missing")
  x <- na_if(x, "N/A")
  x <- na_if(x, "NA")
  x <- na_if(x, "null")
  x <- str_remove_all(x, "%$")
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

messy_ncd_export <- messy_ncd_export %>% mutate(value = clean_value(value))

clean_ncd_data <- messy_ncd_export %>% select(-`str_trim(region)`)

#### Data Analysis ####

## Global trend in premature NCD mortality

trend <- clean_ncd_data %>% filter(indicator == "ncd_mortality_pct", !is.na(value)) %>% group_by(year) %>% 
  summarise(mean_value = mean(value), n_countries = n_distinct(country), .groups = "drop")

ggplot(trend, aes(x = year, y = mean_value)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "black") +
  geom_text(aes(label = paste0("n=", n_countries)), vjust = -1, size = 3, color = "grey40") +
  labs(
    title = "Average premature NCD mortality risk over time",
    subtitle = "Unweighted average across countries with non-missing data that year",
    x = NULL, y = "Probability of premature NCD death (%)",
    caption = "n = number of countries contributing data that year (varies due to missingness)"
  ) + theme_minimal()

## Regional comparison

regional <- clean_ncd_data %>% pivot_wider(names_from = indicator, values_from = value) %>%
  filter(!is.na(ncd_mortality_pct), !is.na(region)) %>%
  group_by(region) %>%
  summarise(mean_value = mean(ncd_mortality_pct), n = n(), .groups = "drop") %>%
  arrange(desc(mean_value))

ggplot(regional, aes(x = reorder(region, mean_value), y = mean_value)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Premature NCD mortality risk by WHO region",
    subtitle = "Averaged across all available country-years in the cleaned data (2018-2022)",
    x = NULL, y = "Probability of premature NCD death (%)"
  ) + theme_minimal()

## Scatter plot for Obesity vs Mortality

scatter_data <- clean_ncd_data %>% pivot_wider(names_from = indicator, values_from = value) %>% 
filter(!is.na(ncd_mortality_pct), !is.na(obesity_pct))

ggplot(scatter_data, aes(x = obesity_pct, y = ncd_mortality_pct, color = region)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  labs(
    title = "Obesity prevalence vs. premature NCD mortality risk",
    subtitle = paste0("All cleaned country-year observations, n = ", nrow(scatter_data)),
    x = "Adult obesity prevalence, age-standardized (%)",
    y = "Probability of premature NCD death (%)",
    color = "WHO region"
  ) + scale_x_continuous(limits = c(10, 30)) + scale_y_continuous(limits = c(25, 45)) + theme_minimal()

## Regression: does obesity + glucose predict mortality, controlling for region?

model_data <- clean_ncd_data %>% pivot_wider(names_from = indicator, values_from = value) %>% 
  filter(if_all(c(ncd_mortality_pct, obesity_pct, raised_glucose_pct, region), ~ !is.na(.))) %>%
  mutate(region = relevel(factor(region), ref = "Europe"))

fit <- lm(ncd_mortality_pct ~ obesity_pct + raised_glucose_pct + region, data = model_data)

tidy_results <- tidy(fit, conf.int = TRUE)
fit_stats <- glance(fit)

write_csv(tidy_results, "C:/Users/USER/OneDrive/Documents/model_coefficients.csv")
write_csv(fit_stats, "C:/Users/USER/OneDrive/Documents/model_fit_stats.csv")

plot(fit)

