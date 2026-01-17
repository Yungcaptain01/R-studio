#### load necessary packages ####

library(tidyverse)
library(readxl)
library(ggpattern)

#### Import data set ####

heart_biomarkers_FYP <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/B.Sc. Project Results/heart biomarkers FYP.xlsx", na = "")

#### clean variable names ####

heart_biomarkers_FYP <- heart_biomarkers_FYP %>% janitor::clean_names()

#### view data set ####

View(heart_biomarkers_FYP)

#### Rename one column ####

colnames(heart_biomarkers_FYP)[colnames(heart_biomarkers_FYP) == "x1"] <- "Group"

#### calculation of mean and standard error of mean (SEM) of Uric Acid ####

Uric_Acid <- heart_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(uric_acid, na.rm = TRUE), 
                                                                    sem = sd(uric_acid, na.rm = TRUE) / sqrt(n()), 
                                                                    .groups = "drop")
                                                                    
#### Visualization of Uric Acid data with ggplot and ggpattern packages ####

ggplot(Uric_Acid, aes(x = Group, y = mean, pattern = Group)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.7,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03
  ) +
  
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    size = 0.8
  ) +
  
  labs(y = "URIC ACID (mg/dl)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5), expand = c(0,0)) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Calculation of Mean and Standard error of mean (SEM) of xanthine oxidase ####

Xanthine_Oxidase <- heart_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(xanthine_oxidase, 
                                                                                       na.rm = TRUE), 
                                                                           sem = sd(xanthine_oxidase, na.rm = TRUE) / sqrt(n()), 
                                                                           .groups = "drop")
 
#### Visualization of xanthine oxidase data with ggplot and ggpattern packages ####                                                                          

ggplot(Xanthine_Oxidase, aes(x = Group, y = mean, pattern = Group)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.7,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03
  ) +
  
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    size = 0.8
  ) +
  
  labs(y = "XANTHINE OXIDASE (u/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Calculation of Mean and Standard error of mean (SEM) of creatine kinase ####

Creatine_Kinase <- heart_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(ck, na.rm = TRUE), 
                                                                    sem = sd(ck, na.rm = TRUE) / sqrt(n()), 
                                                                    .groups = "drop")

#### Visualization of creatine kinase data with ggplot and ggpattern packages ####

ggplot(Creatine_Kinase, aes(x = Group, y = mean, pattern = Group)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.7,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03
  ) +
  
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    linewidth = 0.8
  ) +
  
  labs(y = "CREATINE KINASE (u/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)


#### Calculation of Mean and Standard error of mean (SEM) of Alanine transferase ####

Alanine_transferase <- heart_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(alt, na.rm = TRUE), 
                                                                          sem = sd(alt, na.rm = TRUE) / sqrt(n()), 
                                                                          .groups = "drop")

#### Visualization of Alanine transferase data with ggplot and ggpattern packages ####

ggplot(Alanine_transferase, aes(x = Group, y = mean, pattern = Group)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.7,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03
  ) +
  
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    linewidth = 0.8
  ) +
  
  labs(y = "ALANINE TRANSFERASE (u/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Calculation of Mean and Standard error of mean (SEM) of Superoxide Dismutase ####

Superoxide_dismutase <- heart_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(sod, na.rm = TRUE), 
                                                                              sem = sd(sod, na.rm = TRUE) / sqrt(n()), 
                                                                              .groups = "drop")

#### Visualization of Superoxide Dismutase data with ggplot and ggpattern packages ####

ggplot(Superoxide_dismutase, aes(x = Group, y = mean, pattern = Group)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.7,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03
  ) +
  
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    linewidth = 0.8
  ) +
  
  labs(y = "SUPEROXIDE DISMUTASE (u/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)
