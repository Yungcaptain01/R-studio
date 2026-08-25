## Loading necessary packages

library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(lubridate)
library(writexl)

## Improt data

messy_viral_load_data <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/r4ds/data/messy_viral_load_data.xlsx")

## Clean variables name

new_viral_load <- messy_viral_load_data %>% clean_names()

## Drop fully blank rows

new_viral_load <- new_viral_load %>% filter(if_any(everything(), ~ !is.na(.)))

## Remove white space on all character column

new_viral_load <- new_viral_load %>% mutate(across(where(is.character), str_squish))

## Standardize test case for patient name, clinic site and sample type

new_viral_load <- new_viral_load %>% mutate(
  patient_name = str_to_title(patient_name),
  clinic_site = str_to_title(clinic_site), 
  sample_type = str_to_title(sample_type)
)

## Collapse variants spelling to one spelling on clinic site

clinic_names <- c(
  "Central Clinic" = "Central Clinic", 
  "N. Health Center" = "North Health Center", 
  "North Health Center" = "North Health Center", 
  "Westside Laboratory" = "Westside Lab", 
  "Riverside Hosp" = "Riverside Hospital"
)

new_viral_load <- new_viral_load %>% mutate(clinic_site = recode(clinic_site, !!!clinic_names))

## Remove " -" from virus and convert to uppercase

new_viral_load <- new_viral_load %>% mutate(
  virus = str_to_upper(str_remove_all(virus, "[ -]")), 
  virus = case_when(
    virus == "HIV1" ~ "HIV-1", 
    virus == "HCV" ~ "HCV", 
    virus == "HBV" ~ "HBV", 
    virus == "HEPATITISC" ~ "HCV", 
    virus == "HEPATITISB" ~ "HBV", 
    TRUE ~ virus
  )
)

## Collapse variant spelling to one spelling on sample type

new_viral_load <- new_viral_load %>% mutate(sample_type = case_when(sample_type %in% c("DBS", "Dbs", "Dried Blood Spot") ~ "DBS", TRUE ~ sample_type))

## Standardize assay names

new_viral_load <- new_viral_load %>% mutate(assay = str_to_title(assay))

## Standardize result flag

new_viral_load <- new_viral_load %>% mutate(result_flag = str_to_title(result_flag))

## Remove impossible age values

new_viral_load <- new_viral_load %>% mutate(age = as.numeric(age), age = if_else(age < 0 | age > 110, NA_real_, age))

## Parse collection date from multiple format

new_viral_load <- new_viral_load %>% mutate(collection_date = parse_date_time(collection_date, orders = c("Y-m-d", "m/d/Y", "d-m-Y", "d B Y")
) %>% as_date()) 

## Clean viral load result

new_viral_load <- new_viral_load %>% mutate(viral_load_result = str_remove_all(viral_load_result, "[copies/mL , < Not Detected QNS Target Not Detected TND undetectable Undetectable Error Invalid UNDETECTABLE]"),
                                            viral_load_result = as.numeric(viral_load_result))

## Remove duplicate rows

clean_viral_load <- new_viral_load %>% distinct(patient_id, patient_name, collection_date, .keep_all = TRUE)

## Summary

glimpse(clean_viral_load)
summary(clean_viral_load)

## count missing values in each column

colSums(is.na(clean_viral_load))

## Export clean data

getwd()

write_xlsx(clean_viral_load, "C:/Users/USER/OneDrive/Documents/clean_viral_load.xlsx")
