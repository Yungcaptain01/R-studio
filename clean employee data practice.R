## loading necessary packages

library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(lubridate)
library(writexl)

## Importing data

messy_employee_data <- read_excel("data/messy_employee_data.xlsx")

## Clean column names 

new_employee_data <- messy_employee_data %>% clean_names()

## drop fully blank rows

new_employee_data <- new_employee_data %>% filter(if_any(everything(), ~ !is.na(.)))

## Remove white space on all character column

new_employee_data <- new_employee_data %>% mutate(across(where(is.character), str_squish))

## Standardize text case

new_employee_data <- new_employee_data %>% mutate(
  full_name = str_to_title(full_name), 
  email = str_to_lower(email), 
  city = str_to_title(city), 
  department = str_to_title(department)
)

## standardize city values (collapse variants to one spelling)     

new_employee_data <- new_employee_data %>% mutate(city = case_when(
  city %in% c("Nyc", "New York", "New York City") ~ "New York", 
  city %in% c("La", "L.a.", "Los Angeles") ~ "Los Angeles", 
  city %in% c("Philly", "Philadelphia") ~ "Philadelphia", TRUE ~ city
  ))

## rename specific department values

new_employee_data <- new_employee_data %>% mutate(department = recode(department, "It" = "Information Technology", 
                                                                      "Hr" = "Human Resources"))

## Clean email: mark unknown as na

new_employee_data <- new_employee_data %>% mutate(
  email = na_if(email, "unknown")
)

## clean salary: mark N/A as NA, remove ($ ,) and convert column to numeric

new_employee_data <- new_employee_data %>% mutate(
  salary = str_remove_all(salary, "[$,]"),
  salary = na_if(salary, "N/A"),
  salary = as.numeric(salary)
)

## clean age: treat impossible values as NA

new_employee_data <- new_employee_data %>% mutate(
  age = if_else(age < 16 | age > 100, NA_real_, age), 
  age = as.numeric(age)
)

## Parse join_date from multiple formats into one data type

new_employee_data <- new_employee_data %>%  mutate(
  join_date = parse_date_time(
    join_date,
    orders = c("Y-m-d", "m/d/Y", "d-m-Y", "d B Y")
  ) %>% as_date()
)

## clean satisfaction score: mark out-of-range and N/A as NA

new_employee_data <- new_employee_data %>% mutate(
  satisfaction_score = suppressWarnings(as.numeric(satisfaction_score)),
  satisfaction_score = if_else(satisfaction_score < 1 | satisfaction_score > 5, NA_real_, satisfaction_score)
)

## Remove duplicate rows

new_employee_data <- new_employee_data %>% distinct()

new_employee_data <- new_employee_data %>% distinct(employee_id, .keep_all = TRUE)

## summary

glimpse(new_employee_data)
summary(new_employee_data)

## count remaining missing values per column

colSums(is.na(new_employee_data))

## get working directory to to export clean data

getwd()

## Export cleaned data

write_xlsx(new_employee_data, "C:/Users/USER/OneDrive/Documents/clean_employee_data.xlsx")


