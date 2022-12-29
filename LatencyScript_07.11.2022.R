library(tidyverse)
library(readxl)
library(psych)
library(nlme)
library(ARTool)
library(rstatix)
library(ggpubr)
library(nortest)


LatData <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/Postdoc_UHN/Chenshare/An Introduction to Statistical Learning/Copy_Data For Stats Analysis.xlsx", 
                     sheet = "RevisedLatency", col_names = TRUE)
names(LatData) <- make.names(names(LatData))
LatData$X0WAP <- with(LatData, as.numeric(X0WAP))
LatData$X30mmAP <- with(LatData, as.numeric(X30mmAP))
LatData$X50mmAP <- with(LatData, as.numeric(X50mmAP))


LatData_ver02 <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/Postdoc_UHN/Chenshare/An Introduction to Statistical Learning/Copy_Data For Stats Analysis.xlsx", 
                           sheet = "RevisedLatency_Alt", col_names = TRUE)

LatData_ver02 <- LatData_ver02 %>%
  mutate(Orientation = as.factor(Orientation), Depth = as.factor(Depth), ID = as.factor(ID), Latency=as.numeric(Latency))

# creating a new variable with different coil orientations
PA_Lat <- subset(LatData_ver02, Orientation == "PA")
AP_Lat <- subset(LatData_ver02, Orientation == "AP")
LM_Lat <- subset(LatData_ver02, Orientation == "LM")

Latency_Sham <- subset(LatData_ver02, Depth == "0")
Latency_30mm <- subset(LatData_ver02, Depth == "30")
Latency_50mm <- subset(LatData_ver02, Depth == "50")

# combining with box plots

ggplot (data = LM_Lat, aes(x=Depth, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Lateral-Medial Orientation") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot (data = PA_Lat, aes(x=Depth, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Posterior-Anterior Orientation") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot (data = AP_Lat, aes(x=Depth, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Anterior-Posterior Orientation") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

# plotting by depth

ggplot (data = Latency_Sham, aes(x=Orientation, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Latency_0W") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))


ggplot (data = Latency_30mm, aes(x=Orientation, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Latency_30mm") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot (data = Latency_50mm, aes(x=Orientation, y=Latency)) +
  ylim(15,26) +
  geom_boxplot(color = "red") +
  geom_point () +
  geom_line(aes(group=ID, color = ID)) +
  ggtitle("Latency_50mm") +
  ylab("Latency (ms)") +
  xlab("Depth") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))


# Creating summaries for the subsets
summary_Latency_PA <- describe.by(PA_Lat, "Depth")
summary_Latency_PA
capture.output(summary_Latency_PA, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_RevisedLatency_PA.txt")

summary_Latency_LM <- describe.by(LM_Lat, "Depth")
summary_Latency_LM
capture.output(summary_Latency_LM, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_RevisedLatency_LM.txt")

summary_Latency_AP <- describe.by(AP_Lat, "Depth")
summary_Latency_AP
capture.output(summary_Latency_AP, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_RevisedLatency_AP.txt")

summary_Overall_Latency <- describe.by(LatData_ver02, "Orientation")
summary_Overall_Latency
capture.output(summary_Overall_Latency, file = "C:/Tarun Arora/An Introduction to Statistical Learning/summary_Overall_RevisedLatency.txt")
# Running stats analyses

  # Normality test
Lat_norm = normalityTest(Latency ~ Orientation, test="shapiro.test", 
              data=LatData_ver02)
capture.output(Lat_norm, file = "C:/Tarun Arora/An Introduction to Statistical Learning/NormalityTest_RevisedLatency.txt")


# Non-parametric ARTool (summer's method)  


lat_model <- art(Latency~Depth*Orientation + (1|ID), data=na.omit(LatData_ver02))
lat_model_anova <- anova(lat_model)
print(lat_model_anova, verbose=TRUE)

capture.output(lat_model_anova, file = "C:/Tarun Arora/An Introduction to Statistical Learning/Latency_ARTMainEffects.txt")

# Effect size calculation

F_to_eta2(
  f = c(.041557, 34.904785, .380559),
  df = c(2,2,4),
  df_error = c(114, 114.13, 114)
)

#testing normality of residuals for ANOVA
shapiro.test(residuals(lat_model))
qqnorm(residuals(lat_model)); qqline(residuals(lat_model))

  # Follow-up tests


    # paired-wilcox.test
    wilcoxtest_PAvsAP <- wilcox.test(PA_Lat$Latency,AP_Lat$Latency, paired = TRUE)
    wilcoxtest_PAvsAP
    
    coor_PAvsAP <- cor(PA_Lat$Latency,AP_Lat$Latency, method = c("pearson", "kendall", "spearman"))
    coor_PAvsAP
    
      capture.output(wilcoxtest_PAvsAP, file = "C:/Tarun Arora/An Introduction to Statistical Learning/wilcoxtest_PAvsAP.txt")
    eff_size_PAvsAP <- wilcox_effsize(PA_Lat$Latency ~ AP_Lat$Latency, paired = TRUE)
    
    
    wilcoxtest_LMvsPA <- wilcox.test(LM_Lat$Latency,PA_Lat$Latency, paired = TRUE)
    wilcoxtest_LMvsPA
    capture.output(wilcoxtest_LMvsPA, file = "C:/Tarun Arora/An Introduction to Statistical Learning/wilcoxtest_LMvsPA.txt")
    
    coor_PAvsLM <- cor(PA_Lat$Latency,LM_Lat$Latency, method = c("pearson", "kendall", "spearman"))
    coor_PAvsLM
    
    wilcoxtest_APvsLM <- wilcox.test(AP_Lat$Latency,LM_Lat$Latency, paired = TRUE)
    wilcoxtest_APvsLM
    capture.output(wilcoxtest_APvsLM, file = "C:/Tarun Arora/An Introduction to Statistical Learning/wilcoxtest_APvsLM.txt")
  
    AP_omit = na.omit(AP_Lat$Latency)
    LM_omit = na.omit(LM_Lat$Latency)
    
    coor_APvsLM <- cor(AP_omit,LM_omit, method = c("pearson"))
    coor_APvsLM
    
# cohen's D for paired samples reference = https://www.datanovia.com/en/lessons/t-test-effect-size-using-cohens-d-measure/
    Lat_Difference_PA_AP <- na.omit(PA_Lat$Latency - AP_Lat$Latency)
    Lat_cohenD_PA_AP <- mean(Lat_Difference_PA_AP) / sd(Lat_Difference_PA_AP)
    Lat_cohenD_PA_AP
    
    
    Lat_Difference_PA_LM <- na.omit(PA_Lat$Latency - LM_Lat$Latency)
    Lat_cohenD_PA_LM <- mean(Lat_Difference_PA_LM) / sd(Lat_Difference_PA_LM)
    Lat_cohenD_PA_LM
    
    Lat_Difference_AP_LM <- na.omit(AP_Lat$Latency - LM_Lat$Latency)
    Lat_cohenD_AP_LM <- mean(Lat_Difference_AP_LM) / sd(Lat_Difference_AP_LM)
    Lat_cohenD_AP_LM
    
    
# preparing variables for correlations
    AP4Corr = AP_Lat$Latency
    PA4Corr = PA_Lat$Latency
    LM4Corr = LM_Lat$Latency
    corrData <- data.frame(cbind(AP4Corr, PA4Corr,LM4Corr))
    cor(corrData, use = "pairwise.complete.obs")