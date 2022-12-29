library(tidyverse)
library(readxl)
library(psych)
library(nlme)
library(rstatix)
library(PairedData)
library(lsr)
library(ARTool)
library(rstatix)
library(ggpubr)
library(nortest)


RatioData <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/Postdoc_UHN/Chenshare/An Introduction to Statistical Learning/Copy_Data For Stats Analysis.xlsx", 
                     sheet = "AmplitudeRatio", col_names = TRUE)

RatioData_ver02 <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/Postdoc_UHN/Chenshare/An Introduction to Statistical Learning/Copy_Data For Stats Analysis.xlsx", 
                           sheet = "AmplitudeRatio_Alt", col_names = TRUE)

RatioData_ver02 <- RatioData_ver02 %>%
  mutate(Orientation = as.factor(Orientation), Ratio=as.factor(Ratio), ID = as.factor(ID))

# creating a new variable with different coil orientations
Ratio_30mm <- subset(RatioData_ver02, Ratio == "30mm")
Ratio_50mm <- subset(RatioData_ver02, Ratio == "50mm")

# plotting 
ggplot (data = Ratio_30mm, aes(x=Orientation, y=Amplitude)) +
  geom_boxplot(color = "black") +
  geom_point () +
 # geom_line(aes(group=ID, color = ID)) +
  ggtitle("MEP Amplitude with\n LIFUS @ 30mm Depth") +
  ylab("MEP Amplitude Normalized to Sham") +
  xlab("Coil Orientation") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot (data = Ratio_50mm, aes(x=Orientation, y=Amplitude)) +
  geom_boxplot(color = "black") +
  geom_point () +
  #geom_line(aes(group=ID, color = ID)) +
  ggtitle("MEP Amplitude with\n LIFUS @ 50mm Depth") +
  ylab("MEP Amplitude Normalized to Sham") +
  xlab("Coil Orientation") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

# Creating summaries for the subsets

summary_Ratio30mm <- describeBy(Ratio_30mm, "Orientation")
summary_Ratio50mm <- describeBy(Ratio_50mm, "Orientation")
summary_OverallRatio <- describeBy(RatioData_ver02, "Orientation")

capture.output(summary_Ratio30mm, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_Ratio30mm.txt")
capture.output(summary_Ratio50mm, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_Ratio50mm.txt")
capture.output(summary_OverallRatio, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_OverallRatio.txt")

# Running stats analyses

# Normality test
Ratio_norm <- normalityTest(Ratio ~ Orientation, test="shapiro.test", 
                         data=RatioData_ver02)

# Linear Model

lme_fit_model <- lme(Amplitude ~ Ratio + Orientation + Ratio*Orientation, data=RatioData_ver02, random= ~1|ID, na.action=na.omit)
lme_fit_model
lmeRatioData <- anova(lme_fit_model)
lmeRatioData

# effect size calculation
F_to_eta2(
  f = c(.1826, 4.5119, .1048),
  df = c(1, 2, 2),
  df_error = c(71, 71, 71)
)

eta_squared(lmeRatioData) 

capture.output(lmeRatioData, file = "C:/Tarun Arora/An Introduction to Statistical Learning/LME_Ratios.txt")

# Follow-up tests

# paired-t-test (reference for effect size calculation = https://rcompanion.org/handbook/I_04.html)
Ratio_PA <- subset(RatioData_ver02, Orientation == "PA")
Ratio_AP <- subset(RatioData_ver02, Orientation == "AP")
Ratio_LM <- subset(RatioData_ver02, Orientation == "LM")

ttestRatio_PAvsAP <- t.test(Ratio_PA$Amplitude,Ratio_AP$Amplitude, paired = TRUE)
ttestRatio_PAvsAP
capture.output(ttestRatio_PAvsAP, file = "C:/Tarun Arora/An Introduction to Statistical Learning/ttest_PA_AP.txt")

Difference_PA_AP <- na.omit(Ratio_PA$Amplitude - Ratio_AP$Amplitude)
cohenD_PA_AP <- (mean(Ratio_PA$Amplitude) - mean(na.omit(Ratio_AP$Amplitude))) / sd(Difference_PA_AP)
cohenD_PA_AP


ttest_RatioLMvsPA <- t.test(Ratio_LM$Amplitude,Ratio_PA$Amplitude, paired = TRUE)
ttest_RatioLMvsPA
Difference_LM_PA <- na.omit(Ratio_PA$Amplitude - Ratio_LM$Amplitude)
cohenD_LM_PA <- (mean(Ratio_PA$Amplitude) - mean(na.omit(Ratio_LM$Amplitude))) / sd(Difference_LM_PA)
capture.output(ttest_RatioLMvsPA, file = "C:/Tarun Arora/An Introduction to Statistical Learning/ttest_LM_PA.txt")

ttest_RatioAPvsLM <- t.test(Ratio_AP$Amplitude,Ratio_LM$Amplitude, paired = TRUE)
ttest_RatioAPvsLM
capture.output(ttest_RatioAPvsLM, file = "C:/Tarun Arora/An Introduction to Statistical Learning/ttest_AP_LM.txt")
Difference_AP_LM <- na.omit(Ratio_LM$Amplitude - Ratio_AP$Amplitude)
cohenD_AP_LM <- (mean(Ratio_LM$Amplitude) - mean(na.omit(Ratio_AP$Amplitude))) / sd(Difference_AP_LM)

library("ARTool")

mm1<-art(Amplitude ~   Ratio*Orientation + Error(ID),data=na.omit(RatioData_ver02))
anova(mm1)
