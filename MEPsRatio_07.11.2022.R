# Dealing with MEPs normalized to test-pulse
library(tidyverse)
library(readxl)
library(psych)
library(nlme)
library(ARTool)
# importing Data
RatioData <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/Postdoc_UHN/Chenshare/An Introduction to Statistical Learning/Copy_Data For Stats Analysis.xlsx", 
                       sheet = "AmplitudeRatio", col_names = TRUE)



RatioData_ver02 <- read_xlsx("/Users/tarunarora/Library/CloudStorage/OneDrive-UHN/An Introduction to Statistical Learning/LatencyScript.R/Copy_Data For Stats Analysis.xlsx", 
                                  sheet = "AmplitudeRatio_Alt", col_names = TRUE)

RatioData_ver02 <- RatioData_ver02 %>%
  mutate(Orientation = as.factor(Orientation), Ratio=as.factor(Ratio), ID=as.factor(ID))

RatioData_ver02_Excl <- na.omit(RatioData_ver02)


ggplot(data = RatioData, aes(x = Orientation, y=Amplitude, color=Ratio )) +
geom_point()



# creating subsets comparing different coil orientations

# 30mm

RatioData_30mm <- subset(RatioData_ver02, Ratio == "30mm")

ggplot(data = RatioData_30mm, aes(x = Orientation, y=Amplitude )) +
  geom_boxplot(color = "red") +
  geom_point() +
  geom_line(aes(group = ID, color = ID)) +
  ggtitle("Comparing coil orientation for \n Ratio of MEPs- 30mm:Sham") +
  ylab("Amplitude (mV)") +
  xlab("Orientation") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))


#50mm

RatioData_50mm <- subset(RatioData_ver02, Ratio =="50mm")

ggplot(data = RatioData_50mm, aes(x = Orientation, y=Amplitude)) +
  geom_boxplot(color = "red") +
  geom_point() +
  geom_line(aes(group = ID, color = ID)) +
  ggtitle("Comparing coil orientation for \n Ratio of MEPs- 50mm:Sham") +
  ylab("Amplitude (mV)") +
  xlab("Orientation") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))


# creating subsets comparing different depths

RatioData_PA <- subset(RatioData_ver02, Orientation == "PA")
RatioData_AP <- subset(RatioData_ver02, Orientation == "AP")
RatioData_LM <- subset(RatioData_ver02, Orientation == "LM")

ggplot(data = RatioData_PA, aes(x = Ratio, y=Amplitude)) +
  geom_boxplot(color = "red") +
  geom_point() +
  geom_line(aes(group = ID, color = ID)) +
  ggtitle("PA Orientation \n Comparing LIFUS Depth for Ratio of MEPs") +
  ylab("Amplitude (mV)") +
  xlab("Orientation") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot(data = RatioData_AP, aes(x = Ratio, y=Amplitude)) +
  geom_boxplot(color = "red") +
  geom_point() +
  geom_line(aes(group = ID, color = ID)) +
  ggtitle("AP Orientation \n Comparing LIFUS Depth for Ratio of MEPs") +
  ylab("Amplitude (mV)") +
  xlab("Orientation") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

ggplot(data = RatioData_LM, aes(x = Ratio, y=Amplitude)) +
  geom_boxplot(color = "red") +
  geom_point() +
  geom_line(aes(group = ID, color = ID)) +
  ggtitle("LM Orientation \n Comparing LIFUS Depth for Ratio of MEPs") +
  ylab("Amplitude (mV)") +
  xlab("Orientation") +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold"),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 12, face = "bold"))

# Normality Test

Ratio_norm_Orient = normalityTest(Amplitude ~ Orientation, test="shapiro.test", 
                         data=RatioData_ver02)
Ratio_norm_Depth = normalityTest(Amplitude ~ Ratio, test="shapiro.test", 
                           data=RatioData_ver02)

capture.output(Lat_norm, file = "C:/Tarun Arora/An Introduction to Statistical Learning/NormalityTest_RevisedLatency.txt")

# Regression Analysis (yazan's LME)
lmeRatioData <- anova(lme(Amplitude ~ Ratio + Orientation + Ratio*Orientation,
                        data=RatioData_ver02, random= ~1|ID, na.action=na.omit))
lmeRatioData

capture.output(lmeRatioData, file = "C:/Tarun Arora/An Introduction to Statistical Learning/Ratio_yazanmethodMainEffects.txt")

# paired-t-test
PA_ttestMEP_0vs30 <- t.test(AmpData$`0WPA`, AmpData$`30mmPA`, paired = TRUE)
PA_ttestMEP_0vs30

PA_ttestMEP_0vs50 <- t.test(AmpData$`0WPA`, AmpData$`50mmPA`, paired = TRUE)
PA_ttestMEP_0vs50

AP_ttestMEP_0vs30 <- t.test(AmpData$`0WAP`, AmpData$`30mmAP`, paired = TRUE)
AP_ttestMEP_0vs30

AP_ttestMEP_0vs50 <- t.test(AmpData$`0WAP`, AmpData$`50mmAP`, paired = TRUE)
AP_ttestMEP_0vs50

LM_ttestMEP_0vs30 <- t.test(AmpData$`0WLM`, AmpData$`30mmLM`, paired = TRUE)
LM_ttestMEP_0vs30

LM_ttestMEP_0vs50 <- t.test(AmpData$`0WLM`, AmpData$`50mmLM`, paired = TRUE)
LM_ttestMEP_0vs50