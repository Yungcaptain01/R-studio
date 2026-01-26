#### Loading necessary packages ####

library(readxl)
library(tidyverse)
library(ggpattern)

#### Import serum biomarkers data set from excel package ####

Serum_biomarkers_FYP <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/B.Sc. Project Results/Serum biomarkers FYP.xlsx", na = "")

#### Cleaning variable names ####

Serum_biomarkers_FYP <- janitor::clean_names(dat = Serum_biomarkers_FYP)

#### Remove NA/empty groups before summarise ####

Serum_biomarkers_FYP <- Serum_biomarkers_FYP %>% drop_na()

#### view serum biomarkers dataset ####

View(Serum_biomarkers_FYP)

#### Rename first column to Group ####

Serum_biomarkers_FYP <- Serum_biomarkers_FYP %>% rename(Group = x1)

#### Calculation of mean and standard error of mean (SEM) of insulin ####

Insulin_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(insulin, na.rm = TRUE), 
                                                                    sem = sd(insulin, na.rm = TRUE) / sqrt(n()), 
                                                                    .groups = "drop")

#### Visualization of insulin data with ggplot and ggpattern ####

ggplot(Insulin_, aes(x = Group, y = mean, pattern = Group)) + 
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
  
  labs(title = "Effect of metallic food contaminant on insulin level in serum male wistar rats", 
       y = "INSULIN (µIU/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA for insulin ####

anova_model_insulin <- aov(insulin ~ Group, Serum_biomarkers_FYP)

summary(anova_model_insulin)

#### Run Post-hoc Tukey test for insulin ####

TukeyHSD(anova_model_insulin)

#### Calculation of mean and standard error of mean (SEM) of Total cholesterol (tc) ####

Total_cholesterol <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(tc, na.rm = TRUE), 
                                                                            sem = sd(tc, na.rm = TRUE) / sqrt(n()), 
                                                                            .groups = "drop")


#### Visualization of Total cholesterol data with ggplot and ggpattern ####

ggplot(Total_cholesterol, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Total cholesterol level in serum of male wistar rats", 
       y = "TOTAL CHOLESTEROL (mmol/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(
    aes(label = signif, y = mean + sem + 0.05),
    size = 6
  )

#### Run one way ANOVA test for Total cholestrol ####

anova_model_tc <- aov(tc ~ Group, Serum_biomarkers_FYP)

summary(anova_model_tc)

#### Run Post-hoc Tukey test for total cholesterol ####

TukeyHSD(anova_model_tc)

#### Create a label column for total cholesterol ####

Total_cholesterol$signif <- c(
  "",     # Control
  "",     # Mild
  "*",    # Moderate
  "***"   #severe
)

#### Calculation of mean and standard error of mean (SEM) of Triglycerides (tg) ####

Triglycerides <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(tg, na.rm = TRUE), 
                                                                            sem = sd(tg, na.rm = TRUE) / sqrt(n()), 
                                                                            .groups = "drop")

#### Visualization of Triglycerides data with ggplot and ggpattern ####

ggplot(Triglycerides, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Triglycerides level in serum of male wistar rats", 
       y = "TRIGLYCERIDES (mg/dL)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Triglycerides ####

anova_model_tg <- aov(tg ~ Group, Serum_biomarkers_FYP)
summary(anova_model_tg)

#### Run Post-hoc Tukey test for total cholesterol ####

TukeyHSD(anova_model_tg)

#### Calculation of mean and standard error of mean (SEM) of High density lipoprotein (hdl) ####

High_density_lipoprotein <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(hdl, na.rm = TRUE), 
                                                                        sem = sd(hdl, na.rm = TRUE) / sqrt(n()), 
                                                                        .groups = "drop")

#### Visualization of HDL data with ggplot and ggpattern ####

ggplot(High_density_lipoprotein, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on HDL level in serum of male wistar rats", 
       y = "TOTAL CHOLESTEROL (mg/dL)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for High density lipoprotein ####

anova_model_hdl <- aov(hdl ~ Group, Serum_biomarkers_FYP)
summary(anova_model_hdl)

#### Run Post-hoc Tukey test for high density lipoprotein ####

TukeyHSD(anova_model_hdl)

#### Calculation of mean and standard error of mean (SEM) of Interleukin 6 (IL 6) ####

Interleukin_6 <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(il_6, na.rm = TRUE),
                                                                        sem = sd(il_6, na.rm = TRUE) / sqrt(n()), 
                                                                        .groups = "drop")
                                                                        

#### Visualization of Interleukin 6 data with ggplot and ggpattern ####

ggplot(Interleukin_6, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on IL 6 level in serum of male wistar rats", 
       y = "INTERLEUKIN 6 (pg/mL)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Interleukin 6 ####

anova_model_IL6 <- aov(il_6 ~ Group, Serum_biomarkers_FYP)
summary(anova_model_IL6)

#### Run Post-hoc Tukey test for Interleukin 6 ####

TukeyHSD(anova_model_IL6)

#### Calculation of mean and standard error of mean (SEM) of C-reactive protein (crp) ####

C_reactive_Protein <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(crp, na.rm = TRUE),
                                                                        sem = sd(crp, na.rm = TRUE) / sqrt(n()), 
                                                                        .groups = "drop")      

#### Visualization of C-reactive protein data with ggplot and ggpattern ####

ggplot(C_reactive_Protein, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on CRP level in serum of male wistar rats", 
       y = "C-reactive protein (mg/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for C Reactive Protein ####

anova_model_Crp <- aov(crp ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Crp)

#### Run Post-hoc Tukey test for C Reactive Protein ####

TukeyHSD(anova_model_Crp)

#### Calculation of mean and standard error of mean (SEM) of xanthine oxidase ####

Xanthine_O <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(xanthine_oxidase, na.rm = TRUE),
                                                                     sem = sd(xanthine_oxidase, na.rm = TRUE) / sqrt(n()), 
                                                                     .groups = "drop")

#### Visualization of xanthine oxidase data with ggplot and ggpattern ####

ggplot(Xanthine_O, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on  xanthine oxidase level in serum of male wistar rats", 
       y = "XANTHINE OXIDASE (u/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for xanthine oxidase #### 

anova_model <- aov(xanthine_oxidase ~ Group, Serum_biomarkers_FYP)

summary(anova_model)

#### Run Post-hoc Tukey test for xanthine oxidase ####

TukeyHSD(anova_model)

#### Calculation of mean and standard error of mean (SEM) of Malondialdehyde (MDA) ####

Malondialdehyde <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(mda, na.rm = TRUE), 
                                                                                   sem = sd(mda, na.rm = TRUE) / sqrt(n()), 
                                                                                   .groups = "drop")

#### Visualization of Malondialdehyde data with ggplot and ggpattern ####

ggplot(Malondialdehyde, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Malondialdehyde level in serum of male wistar rats", 
       y = "MALONDIALDEHYDE (umol/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for malondialdehyde #### 

anova_model_Mda <- aov(mda ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Mda)

#### Run Post-hoc Tukey test for Malondialdehyde ####

TukeyHSD(anova_model_Mda)

#### Calculation of mean and standard error of mean (SEM) of Glucose-6-phosphate dehydrogenase (g6pdh) ####

G_6_P_D_H <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(g6pdh, na.rm = TRUE), 
                                                                          sem = sd(g6pdh, na.rm = TRUE) / sqrt(n()), 
                                                                          .groups = "drop")

#### Visualization of G6PDH data with ggplot and ggpattern ####

ggplot(G_6_P_D_H, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on G6PDH level in serum of male wistar rats", 
       y = "G6PDH (umol/ml)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for G6PDH #### 

anova_model_G6PDH <- aov(g6pdh ~ Group, Serum_biomarkers_FYP)

summary(anova_model_G6PDH)

#### Run Post-hoc Tukey test for G6PDH ####

TukeyHSD(anova_model_G6PDH)

#### Calculation of mean and standard error of mean (SEM) of Glutathione (GSH) ####

Glutathione <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(gsh, na.rm = TRUE), 
                                                                          sem = sd(gsh, na.rm = TRUE) / sqrt(n()), 
                                                                          .groups = "drop")

#### Visualization of Glutathione data with ggplot and ggpattern ####

ggplot(Glutathione, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Glutathione level in serum of male wistar rats", 
       y = "GSH (umol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for G6PDH #### 

anova_model_GSH <- aov(gsh ~ Group, Serum_biomarkers_FYP)

summary(anova_model_GSH)

#### Run Post-hoc Tukey test for G6PDH ####

TukeyHSD(anova_model_GSH)

#### Calculation of mean and standard error of mean (SEM) of Protein ####

Protein_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(protein, na.rm = TRUE), 
                                                                      sem = sd(protein, na.rm = TRUE) / sqrt(n()), 
                                                                      .groups = "drop")

#### Visualization of Protein data with ggplot and ggpattern ####

ggplot(Protein_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Protein level in serum of male wistar rats", 
       y = "Protein (g/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Protein #### 

anova_model_protein <- aov(protein ~ Group, Serum_biomarkers_FYP)

summary(anova_model_protein)

#### Run Post-hoc Tukey test for protein ####

TukeyHSD(anova_model_protein)

#### Calculation of mean and standard error of mean (SEM) of Glutathione Peroxidase (gpx) ####

Glutathione_Peroxidase <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(gpx, na.rm = TRUE), 
                                                                   sem = sd(gpx, na.rm = TRUE) / sqrt(n()), 
                                                                   .groups = "drop")

#### Visualization of Glutathione peroxidase data with ggplot and ggpattern ####

ggplot(Glutathione_Peroxidase, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Glutathione Peroxidase level in serum of male wistar rats", 
       y = "GPX (U/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Glutathione Peroxidase #### 

anova_model_Gpx <- aov(gpx ~ Group, Serum_biomarkers_FYP)

summary(anova_model_protein)

#### Run Post-hoc Tukey test for Glutathione peroxidase ####

TukeyHSD(anova_model_Gpx)

#### Calculation of mean and standard error of mean (SEM) of catalase ####

Catalase_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(catalase, na.rm = TRUE), 
                                                                                 sem = sd(catalase, na.rm = TRUE) / sqrt(n()), 
                                                                                 .groups = "drop")

#### Visualization of catalase data with ggplot and ggpattern ####

ggplot(Catalase_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on catalase level in serum of male wistar rats", 
       y = "CATALASE (U/mL)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Catalase #### 

anova_model_Catalase <- aov(catalase ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Catalase)

#### Run Post-hoc Tukey test for Catalase ####

TukeyHSD(anova_model_Catalase)

#### Calculation of mean and standard error of mean (SEM) of Superoxide Dismutase (sod) ####

Superoxide_dismutase <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(sod, na.rm = TRUE), 
                                                                                 sem = sd(sod, na.rm = TRUE) / sqrt(n()), 
                                                                                 .groups = "drop")

#### Visualization of Superoxide Dismutase data with ggplot and ggpattern ####

ggplot(Superoxide_dismutase, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Superoxide Dismutase level in serum of male wistar rats", 
       y = "SOD (U/mL)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Superoxide Dismutase #### 

anova_model_SOD <- aov(sod ~ Group, Serum_biomarkers_FYP)

summary(anova_model_SOD)

#### Run Post-hoc Tukey test for Superoxide Dismutase ####

TukeyHSD(anova_model_SOD)

#### Calculation of mean and standard error of mean (SEM) of Creatinine ####

Creatinine_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(creatinine, na.rm = TRUE), 
                                                                                 sem = sd(creatinine, na.rm = TRUE) / sqrt(n()), 
                                                                                 .groups = "drop")

#### Visualization of Creatinine data with ggplot and ggpattern ####

ggplot(Creatinine_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Creatinine level in serum of male wistar rats", 
       y = "CREATININE (nmol/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Creatinine #### 

anova_model_Creatinine <- aov(creatinine ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Creatinine)

#### Run Post-hoc Tukey test for creatinine ####

TukeyHSD(anova_model_Creatinine)

#### Calculation of mean and standard error of mean (SEM) of Urea ####

Urea_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(urea, na.rm = TRUE), 
                                                                                 sem = sd(urea, na.rm = TRUE) / sqrt(n()), 
                                                                                 .groups = "drop")

#### Visualization of Urea data with ggplot and ggpattern ####

ggplot(Urea_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Urea level in serum of male wistar rats", 
       y = "Urea (mg/dl)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(
    aes(label = signif, y = mean + sem + 2),
    size = 6
  )

#### Create a label column for Urea ####

Urea_$signif <- c(
  "",     # Control
  "",     # Mild
  "*",    # Moderate
  "***"   #severe
)

#### Run one way ANOVA test for Urea #### 

anova_model_Urea <- aov(urea ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Urea)

#### Run Post-hoc Tukey test for urea ####

TukeyHSD(anova_model_Urea)

#### Calculation of mean and standard error of mean (SEM) of Lactate Dehydrogenase (ldh) ####

Lactate_dehydrogenase <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(ldh, na.rm = TRUE), 
                                                                      sem = sd(ldh, na.rm = TRUE) / sqrt(n()), 
                                                                      .groups = "drop")

#### Visualization of Lactate Dehydrogenase data with ggplot and ggpattern ####

ggplot(Lactate_dehydrogenase, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Lactate Dehydrogenase level in serum of male wistar rats", 
       y = "LDH (U/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(
    aes(label = signif, y = mean + sem + 30),
    size = 6
  )

#### Create a label column for Lactate Dehydrogenase ####

Lactate_dehydrogenase$signif <- c(
  "",     # Control
  "**",   # Mild
  "**",   # Moderate
  "**"    #severe
)

#### Run one way ANOVA test for Lactate Dehydrogenase #### 

anova_model_LDH <- aov(ldh ~ Group, Serum_biomarkers_FYP)

summary(anova_model_LDH)

#### Run Post-hoc Tukey test for Lactate Dehydrogenase ####

TukeyHSD(anova_model_LDH)

#### Calculation of mean and standard error of mean (SEM) of Aspartate Aminotransferase (ast) ####

Aspartate_aminostransferase <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(ast, na.rm = TRUE), 
                                                                      sem = sd(ast, na.rm = TRUE) / sqrt(n()), 
                                                                      .groups = "drop")

#### Visualization of Aspartate aminotransferase data with ggplot and ggpattern ####

ggplot(Aspartate_aminostransferase, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on AST level in serum of male wistar rats", 
       y = "AST (U/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Aspartate aminotransferase #### 

anova_model_AST <- aov(ast ~ Group, Serum_biomarkers_FYP)

summary(anova_model_AST)

#### Run Post-hoc Tukey test for Aspartate aminotransferase ####

TukeyHSD(anova_model_AST)

#### Calculation of mean and standard error of mean (SEM) of Alanine aminotransferase ####

Alanine_aminotransferase <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(alt, na.rm = TRUE), 
                                                                      sem = sd(alt, na.rm = TRUE) / sqrt(n()), 
                                                                      .groups = "drop")

#### Visualization of Alanine aminotransferase data with ggplot and ggpattern ####

ggplot(Alanine_aminotransferase, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on ALT level in serum of male wistar rats", 
       y = "ALT (U/L)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(aes(label = signif, y = mean + sem + 5), 
                                                           size = 6)

#### Create a Label column for Alanine aminotransferase ####

Alanine_aminotransferase$signif <- c(
  "",   #control
  "",   #mild
  "**", #moderate
  "**"  #severe
)

#### Run one way ANOVA test for Alanine aminotransferase #### 

anova_model_ALT <- aov(alt ~ Group, Serum_biomarkers_FYP)

summary(anova_model_ALT)

#### Run Post-hoc Tukey test for Alanine aminotransferase ####

TukeyHSD(anova_model_ALT)

#### Calculation of mean and standard error of mean (SEM) of Lactate ####

Lactate_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(lactate, na.rm = TRUE), 
                                                                                   sem = sd(lactate, na.rm = TRUE) / sqrt(n()), 
                                                                                   .groups = "drop")

#### Visualization of Lactate data with ggplot and ggpattern ####

ggplot(Lactate_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Lactate level in serum of male wistar rats", 
       y = "Lactate (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(aes(label = signif, y = mean + sem + 0.1), 
                                                           size = 6)

#### Create a Label column for Lactate ####

Lactate_$signif <- c(
  "",    #control
  "***", #mild
  "",    #moderate
  ""     #severe
)

#### Run one way ANOVA test for Lactate #### 

anova_model_Lactate <- aov(lactate ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Lactate)

#### Run Post-hoc Tukey test for Lactate ####

TukeyHSD(anova_model_Lactate)

#### Calculation of mean and standard error of mean (SEM) of Uric Acid ####

Uric_Acid <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(uric_acid, na.rm = TRUE), 
                                                                                  sem = sd(uric_acid, na.rm = TRUE) / sqrt(n()), 
                                                                                  .groups = "drop")

#### Visualization of Uric Acid data with ggplot and ggpattern ####

ggplot(Uric_Acid, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Uric Acid level in serum of male wistar rats", 
       y = "URIC ACID (mg/dl)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(aes(label = signif, y = mean + sem + 1), 
                                                           size = 6)

#### Create a Label column for Uric Acid ####

Uric_Acid$signif <- c(
  "",   #control
  "*",   #mild
  "", #moderate
  ""  #severe
)

#### Run one way ANOVA test for Uric Acid #### 

anova_model_Uric_Acid_ <- aov(uric_acid ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Uric_Acid_)

#### Run Post-hoc Tukey test for Uric Acid ####

TukeyHSD(anova_model_Uric_Acid_)

#### Calculation of mean and standard error of mean (SEM) of Phosphorus ####

Phosphorus_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(phosphorus, na.rm = TRUE), 
                                                                    sem = sd(phosphorus, na.rm = TRUE) / sqrt(n()), 
                                                                    .groups = "drop")

#### Visualization of Phosphorus data with ggplot and ggpattern ####

ggplot(Phosphorus_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Phosphorus level in serum of male wistar rats", 
       y = "PHOSPHORUS (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Uric Acid #### 

anova_model_Phosphorus__ <- aov(phosphorus ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Phosphorus__)

#### Run Post-hoc Tukey test for Uric Acid ####

TukeyHSD(anova_model_Phosphorus__)

#### Calculation of mean and standard error of mean (SEM) of Calcium ####

Calcium_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(calcium, na.rm = TRUE), 
                                                                      sem = sd(calcium, na.rm = TRUE) / sqrt(n()), 
                                                                      .groups = "drop")

#### Visualization of Calcium data with ggplot and ggpattern ####

ggplot(Calcium_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Calcium level in serum of male wistar rats", 
       y = "CAlCIUM (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE) + geom_text(aes(label = signif, y = mean + sem + 0.1), 
                                                           size = 6)

#### Create a Label column for Calcium ####

Calcium_$signif <- c(
  "",   #control
  "",   #mild
  "*", #moderate
  "**"  #severe
)

#### Run one way ANOVA test for Calcium #### 

anova_model_Calcium__ <- aov(calcium ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Calcium__)

#### Run Post-hoc Tukey test for Calcium ####

TukeyHSD(anova_model_Calcium__)

#### Calculation of mean and standard error of mean (SEM) of Magnesium ####

Magnesium_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(magnesium, na.rm = TRUE), 
                                                                   sem = sd(magnesium, na.rm = TRUE) / sqrt(n()), 
                                                                   .groups = "drop")

#### Visualization of Magnesium data with ggplot and ggpattern ####

ggplot(Magnesium_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Magnesium level in serum of male wistar rats", 
       y = "MAGNESIUM (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Magnesium #### 

anova_model_Magnesium__ <- aov(magnesium ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Magnesium__)

#### Run Post-hoc Tukey test for Magnesium ####

TukeyHSD(anova_model_Magnesium__)

#### Calculation of mean and standard error of mean (SEM) of Potassium ####

Potassium_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(potassium, na.rm = TRUE), 
                                                                     sem = sd(potassium, na.rm = TRUE) / sqrt(n()), 
                                                                     .groups = "drop")

#### Visualization of Potassium data with ggplot and ggpattern ####

ggplot(Potassium_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Potassium level in serum of male wistar rats", 
       y = "POTASSIUM (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Potassium #### 

anova_model_Potassium__ <- aov(potassium ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Potassium__)

#### Run Post-hoc Tukey test for Potassium ####

TukeyHSD(anova_model_Potassium__)

#### Calculation of mean and standard error of mean (SEM) of Sodium ####

Sodium_ <- Serum_biomarkers_FYP %>% group_by(Group) %>% summarise(mean = mean(sodium, na.rm = TRUE), 
                                                                     sem = sd(sodium, na.rm = TRUE) / sqrt(n()), 
                                                                     .groups = "drop")

#### Visualization of Sodium data with ggplot and ggpattern ####

ggplot(Sodium_, aes(x = Group, y = mean, pattern = Group)) + 
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    color = "black",
    width = 0.5,
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
  
  labs(title = "Effect of metallic food contaminant on Sodium level in serum of male wistar rats", 
       y = "SODIUM (mmol/l)", x = "GROUP") +
  theme_classic(base_size = 12) + scale_x_discrete(na.translate = FALSE) + 
  scale_pattern_discrete(na.translate = FALSE)

#### Run one way ANOVA test for Sodium #### 

anova_model_Sodium__ <- aov(sodium ~ Group, Serum_biomarkers_FYP)

summary(anova_model_Sodium__)

#### Run Post-hoc Tukey test for Sodium ####

TukeyHSD(anova_model_Sodium__)
