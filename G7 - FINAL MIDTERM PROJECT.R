# # GROUP 7 MIDTERM PROJECT ##########################################################
# Members:
# Quinto, Raven Luke
# Lawrenz Mikael Alcaparaz
# Ricamae Calugtong
# Rachelle Cunanan

# 1. CHOOSE A DATASET #####################################################################
# The dataset chosen is the Framingham Heart Study about Risks of Coronary Heart Diseases

# 2.A. R BASICS - SETTING UP
print("R Version:")
print(R.version)
print(sessionInfo())

# do only if there's issues with the directory
#getwd() # checks your directory
#setwd("your-file-directory-here-containing-the-csvfile")

# 2.B. DATA IMPORT AND PREPROCESSING
library(dplyr)
file_path <- "framingham_heart_study.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

data <- rename(data,sex = male)

missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

if ("cigsPerDay" %in% colnames(data)) {
  data$cigsPerDay[is.na(data$cigsPerDay)] <- 0
}
if ("BPMeds" %in% colnames(data)) {
  data$BPMeds[is.na(data$BPMeds)] <- 0
}
if("education" %in% colnames(data)){
  data$education[is.na(data$education)] <- 0
}
columns_to_exclude_na <- c("totChol", "BMI", "heartRate", "glucose")
data <- data[complete.cases(data[, columns_to_exclude_na]), ]

# 2.C. DATA EXPLORATION AND AND DESCRIPTIVE STATISTICS###############################
# Summarize the dataset using descriptive statistics.
summary(data)

# Explore the structure of the dataset (str(), head(), tail()).
str(data)
head(data)
tail(data)

#Use R to calculate key descriptive measures (mean, variance, correlation, etc.)
numeric_cols <- sapply(data, is.numeric)
means <- sapply(data[, numeric_cols], mean, na.rm = TRUE)
print(means)

medians <- sapply(data[, numeric_cols], median, na.rm = TRUE)
print(medians)

standard_dev <- sapply(data[, numeric_cols], sd, na.rm = TRUE)
print(standard_dev)

variances <- sapply(data[, numeric_cols], var, na.rm = TRUE)
print(variances)

correlation <- cor(data[, numeric_cols], use = "complete.obs")
print(correlation)

# Making a dataframe summary (Optional)
summary_stats <- data.frame(
  Variable = names(means),
  Mean = means,
  Median = medians,
  SD = standard_dev,
  Variance = variances,
  row.names = NULL
)
summary_stats

# DATA VISUALIZATION ##############################################################
# install the following if needed:
# install.packages("showtext")
#install.packages("tidyr")

library(ggplot2)
library(showtext)
library(tidyr)
font_add_google("Roboto","roboto")
showtext_auto()

# SEX / MALE
# 1 means male, 0 means female
sexCount = table(data$sex)
sexData = as.data.frame(sexCount)
colnames(sexData)<-c("sex","count")

ggplot(sexData,aes(x=sex, y=count,fill=factor(sex,labels = c("Female","Male")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="Sex Demographics",x="Sex",y="Count",fill="Sex") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="Female (0)","1"="Male (1)")) +
  # for color
  scale_fill_manual(values = c("Female" = "pink","Male" = "skyblue")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$sex) * sum(sexData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(data$sex) + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,2500) + 
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# AGE ############################################################################
ggplot(data, aes(x = age)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 5, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(age), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(age), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Age Distribution", x = "Age", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(10, 100, by = 5)) +
  stat_bin(binwidth=5, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="#2E282A",size = 4, fontface="bold",family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# EDUCATION #######################################################################
eduCount = table(data$education)
eduData = as.data.frame(eduCount)
colnames(eduData)<-c("education","count")

ggplot(eduData,aes(x=education, y=count,fill=education)) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="Education Level of Patients",x="Education Level",y="Count",fill="Education") + 
  # for color
  scale_fill_manual(values = c("0" = "antiquewhite3","1" = "aquamarine","2" = "aquamarine4","3" = "deepskyblue3","4" = "deepskyblue4")) +
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(data$education), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(data$education), color = "Median"), linetype = "dashed",linewidth=1) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  guides(color = guide_legend(title = "Stats")) +
  # adjust y limit or max
  ylim(0,2000) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )
  
# CURRENTSMOKER ##################################################################
# 1 - yes, 0 - no

smokerCount = table(data$currentSmoker)
smokerData = as.data.frame(smokerCount)
colnames(smokerData)<-c("currentSmoker","count")

ggplot(smokerData,aes(x=currentSmoker, y=count,fill=factor(currentSmoker,labels = c("No","Yes")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="Smoking Status of Patients",x="Smoking Status",y="Count",fill="Smoking?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$currentSmoker) * sum(smokerData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_hline(aes(yintercept = median(data$currentSmoker) * sum(smokerData$count), color = "Median"), linetype = "dashed", linewidth = 1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,2500)+
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# CIGSPERDAY #####################################################################
ggplot(data, aes(x = cigsPerDay)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 20,  fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(cigsPerDay), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(cigsPerDay), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Cigarettes Per Day", x = "Cigarettes Per Day", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  stat_bin(binwidth=20, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# BPMEDS #########################################################################
# 1 - yes, 0 - no

bpmedsCount = table(data$BPMeds)
bpmedsData = as.data.frame(bpmedsCount)
bpMedian = median(data$BPMeds)
colnames(bpmedsData)<-c("bpmeds","count")

ggplot(bpmedsData,aes(x=bpmeds, y=count,fill=factor(count,labels = c("Yes","No")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="Usage of BP Medications by Patients",x="BP Meds Use",y="Count",fill="BP Meds?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$BPMeds) * sum(bpmedsData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = bpMedian + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,4000)

# PREVALENT STROKE ################################################################
# 1 - yes, 0 - no

prevStrokeCount = table(data$prevalentStroke)
strokeData = as.data.frame(prevStrokeCount)
strokeMedian = median(data$prevalentStroke)
colnames(strokeData)<-c("stroke","count")

ggplot(strokeData,aes(x=stroke, y=count,fill=factor(count,labels = c("Yes","No")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="History of Stroke in Patients",x="Prevalence of Stroke",y="Count",fill="Had Stroke?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$prevalentStroke) * sum(strokeData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = bpMedian + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,4000)

# PREVALENT HYPERTENSION #########################################################
# 1 - yes, 0 - no

prevHypCount = table(data$prevalentHyp)
hypData = as.data.frame(prevHypCount)
hypMedian = median(data$prevalentHyp)
colnames(hypData)<-c("hyp","count")

ggplot(hypData,aes(x=hyp, y=count,fill=factor(count,labels = c("Yes","No")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="History of Hypertension in Patients",x="Prevalence of Hypertension",y="Count",fill="Had Hypertension?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$prevalentHyp) * sum(hypData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = hypMedian + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,3000)

# DIABETES #######################################################################
# 1 - yes, 0 - no

diabetesCount = table(data$diabetes)
diabetesData = as.data.frame(diabetesCount)
diabetesMedian = median(data$diabetes)
colnames(diabetesData)<-c("diabetes","count")

ggplot(diabetesData,aes(x=diabetes, y=count,fill=factor(count,labels = c("Yes","No")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="History of Diabetes in Patients",x="Prevalence of Diabetes",y="Count",fill="Had Diabetes?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$diabetes) * sum(diabetesData$count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = diabetesMedian + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,4000)

# TOTCHOL #######################################################################
ggplot(data, aes(x = totChol)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 50, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(totChol), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(totChol), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Total Cholesterol Levels", x = "Total Cholesterol Level", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(100, 700, by = 100)) +
  stat_bin(binwidth=50, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# SYSBP ##########################################################################
ggplot(data, aes(x = sysBP)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 50, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(sysBP), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(sysBP), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Systolic Blood Pressure of Patients", x = "Systolic BP", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(100, 700, by = 50)) +
  stat_bin(binwidth=50, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )
  
# DIABP ##########################################################################
ggplot(data, aes(x = diaBP)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 50, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(diaBP), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(diaBP), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Diastolic Blood Pressure of Patients", x = "Diastolic BP", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(0, 700, by = 50)) +
  stat_bin(binwidth=50, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto") +
  ylim(0,3000) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )
  
# BMI ###############################################################################
ggplot(data, aes(x = BMI)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 5, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(BMI), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(BMI), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Body Mass Index of Patients", x = "Body Mass Index (BMI)", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(0, 700, by = 5)) +
  stat_bin(binwidth=5, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto")
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# HEART RATE #######################################################################
ggplot(data, aes(x = heartRate)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 10, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(heartRate), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(heartRate), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Heart Rate of Patients", x = "Heart Rate (bpm)", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(0, 700, by = 20)) +
  stat_bin(binwidth=10, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto")
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )
  
# GLUCOSE ##########################################################################
ggplot(data, aes(x = glucose)) +
  # adjust binwidth according to how much of each data point you want to show
  geom_histogram(binwidth = 50, fill = "#CAFE48", color = "#2E282A") +
  # for showing the mean and median
  geom_vline(aes(xintercept = mean(glucose), color = "Mean"), linetype = "dashed",linewidth=1) +
  geom_vline(aes(xintercept = median(glucose), color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  # for labels
  labs(title = "Glucose Level of Patients", x = "Glucose Level", y = "Frequency", color = "Stats") +
  # for x labels
  scale_x_continuous(breaks = seq(0, 700, by = 50)) +
  stat_bin(binwidth=50, geom='text', aes(label=..count..), position=position_stack(vjust=0.5), color="black",size = 4, fontface="bold",family = "roboto")
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )
  
# RISK OF TENYEARCHD #####################################################################
# 1 - yes, 0 - no

chdCount = table(data$TenYearCHD)
chdData = as.data.frame(chdCount)
chdMedian = median(data$TenYearCHD)
colnames(chdData)<-c("chd","count")

ggplot(chdData,aes(x=chd, y=count,fill=factor(count,labels = c("Yes","No")))) +
  geom_bar(stat = "identity") +
  # for labels  
  labs(title="Risk of Coronary Heart Disease (CHD)",x="Risk of CHD",y="Count",fill="CHD Risk in 10 Years?") + 
  # for bar labels 
  scale_x_discrete(labels=c("0"="No (0)","1"="Yes (1)")) +
  # for color
  scale_fill_manual(values = c("No" = "darkolivegreen3","Yes" = "coral")) + 
  # Show mean and median as horizontal lines
  geom_hline(aes(yintercept = mean(data$TenYearCHD) * sum(count), color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = chdMedian + 1, color = "Median"), linetype = "dashed",linewidth=1) +
  # for color
  scale_color_manual(values = c("Mean" = "darkturquoise", "Median" = "deeppink")) +
  guides(color = guide_legend(title = "Stats")) +
  # show text for each label 
  geom_text(aes(label = count), vjust = -0.5,size = 4, fontface="bold",family = "roboto") + 
  # adjust y limit or max
  ylim(0,4000)

# MEAN OVERALL GRAPHS #############################################################
library(ggplot2)
library(tidyr)

# MEAN, MEDIAN, SD GRAPHS #########################################################
# SEX, AGE, EDUC ##################################################################
sexAgeEduData = data.frame(means = means[c("sex", "age", "education")],
                     medians = medians[c("sex", "age", "education")],
                     sd = standard_dev[c("sex", "age", "education")])
sexAgeEduData$Variable = rownames(sexAgeEduData)
saeLongData = pivot_longer(sexAgeEduData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(saeLongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Sex, Age, Education - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,55) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# current smoker, cigs per day ################################################################
smokerCigsData = data.frame(means = means[c("currentSmoker", "cigsPerDay")],
                           medians = medians[c("currentSmoker", "cigsPerDay")],
                           sd = standard_dev[c("currentSmoker", "cigsPerDay")])
smokerCigsData$Variable = rownames(smokerCigsData)
smokerCigsLongData = pivot_longer(smokerCigsData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(smokerCigsLongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "CurrentSmoker, Cigs Per Day - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,15) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# bp meds, prev stroke, hyp ################################################################
bpData = data.frame(means = means[c("BPMeds", "prevalentStroke", "prevalentHyp")],
                            medians = medians[c("BPMeds", "prevalentStroke", "prevalentHyp")],
                            sd = standard_dev[c("BPMeds", "prevalentStroke", "prevalentHyp")])
bpData$Variable = rownames(bpData)
bpLongData = pivot_longer(bpData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(bpLongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "BPMeds, prevStroke, prevHyp - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,0.6) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# diabetes, glucose, totchol ################################################################
dgcData = data.frame(means = means[c("diabetes", "glucose", "totChol")],
                    medians = medians[c("diabetes", "glucose", "totChol")],
                    sd = standard_dev[c("diabetes", "glucose", "totChol")])
dgcData$Variable = rownames(dgcData)
dgcLongData = pivot_longer(dgcData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(dgcLongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Diabetes, Glucose, TotChol - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,500) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# sysbp, diabp, bmi, heartrate ################################################################
hrBMIData = data.frame(means = means[c("sysBP", "diaBP", "heartRate","BMI")],
                     medians = medians[c("sysBP", "diaBP", "heartRate","BMI")],
                     sd = standard_dev[c("sysBP", "diaBP", "heartRate","BMI")])
hrBMIData$Variable = rownames(hrBMIData)
hrBMILongData = pivot_longer(hrBMIData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(hrBMILongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "SysBP, DiaBP, Heart Rate, BMI - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,500) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# tenyearchd ########################################################################
chdData = data.frame(means = means[c("TenYearCHD")],
                       medians = medians[c("TenYearCHD")],
                       sd = standard_dev[c("TenYearCHD")])
chdData$Variable = rownames(chdData)
chdLongData = pivot_longer(chdData, cols=c(means,medians,sd),names_to = "Statistics",values_to = "Value")

ggplot(chdLongData, aes(x = Variable, y = Value, fill = Statistics)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "TenYearCHD - Mean Median, SD",
       x = "Variable",
       y = "Value") +
  coord_flip() +  # Flip x and y axes
  scale_fill_manual(values = c("means" = "#0B3954","medians" = "#FF6663","sd" = "#EDAE49"),
                    labels = c("Mean","Median","Standard Deviation")) + 
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), hjust = -0.2,size = 4, fontface="bold",family = "roboto") +  # Adding text labels
  ylim(0,0.5) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# FOR THE PRESENTATION ##############################################################
# AGE & EDUC #######################################################################
ageEduMean <- data %>%
  group_by(education) %>%
  summarize(mean_age = mean(age))  # Calculate mean age
ageEduMean$education <- as.factor(ageEduMean$education)

ggplot(ageEduMean, aes(x = education, y = mean_age, fill = education)) +
  geom_bar(stat = "identity", color = "black") +  # Use identity for precomputed values
  # Add labels
  labs(title = "Mean Age by Education Level", x = "Education Level", y = "Mean Age", fill="Education Level") +
  scale_fill_manual(values = c("#B3C2F2", "#735CDD", "#9000B3", "#7E007B", "#37000A")) +
  # Add text labels for the mean values
  geom_text(aes(label = round(mean_age, 2)), vjust = -0.5, size = 4, fontface = "bold", family = "roboto") +
  # Custom theme
  ylim(0,60) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# currentsmoker & cigs per day ######################################################
ggplot(data, aes(x = cigsPerDay, fill = factor(currentSmoker))) +
  geom_histogram(binwidth = 50, position = "dodge", color = "black", alpha = 0.6) +
  labs(title = "Distribution of Cigarettes Per Day by Smoking Status",
       x = "Cigarettes Per Day", y = "Count", fill="Smoking?") +
  scale_fill_manual(values = c("darkolivegreen3", "salmon"),
                    labels = c("No", "Yes")) +
  stat_bin(binwidth = 50, geom = 'text', aes(label = ..count..), 
           position = position_dodge(width = 50), vjust=-1.1, color = "black", 
           size = 4, fontface = "bold", family = "roboto") + 
  ylim(0,2500) + 
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# bpmeds, prev stroke, hyp ######################################################
mean_data <- data %>%
  group_by(BPMeds) %>%
  summarize(
    mean_stroke = mean(prevalentStroke),  # Calculate mean of prevalentStroke
    mean_hyp = mean(prevalentHyp)         # Calculate mean of prevalentHyp
  )

# reshape
mean_data_long <- mean_data %>%
  pivot_longer(cols = c(mean_hyp,mean_stroke), names_to = "Condition", values_to = "Mean")

ggplot(mean_data_long, aes(x = BPMeds, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = "stack", color="black") +  # Create stacked bars
  labs(title = "Mean of Prevalent Stroke and Hypertension by BP Meds Status",
       x = "BP Meds Status (1 = Yes, 0 = No)", y = "Mean Value") +
  scale_fill_manual(values = c("mean_stroke" = "brown1", "mean_hyp" = "#CAFE48"),
                    labels = c("Mean Hypertension","Mean Stroke")) + 
  ylim(0,1.5) +
  geom_text(aes(label = round(Mean, 2)), position = position_nudge(y = 0.1), size = 4,fontface = "bold",family="roboto") + 
theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# diabetes, glucose, totchol #####################################################
dcg_mean_data <- data %>%
  select(diabetes, glucose, totChol) %>%
  pivot_longer(cols = c(glucose, totChol), 
               names_to = "Variable", 
               values_to = "Value") %>%
  group_by(diabetes, Variable) %>%
  summarize(Mean = mean(Value, na.rm = TRUE))

# Create the bar chart
ggplot(dcg_mean_data, aes(x = factor(diabetes), y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color="black") + 
  # Add labels for each bar showing the mean
  geom_text(aes(label = round(Mean, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4, fontface = "bold", family = "roboto") +
  labs(title = "Mean of Glucose and Total Cholesterol by Diabetes Status",
       x = "Diabetes (1 = Yes, 0 = No)", y = "Mean Value",
       fill = "Variable") +
  scale_fill_manual(values = c("glucose" = "lightblue", "totChol" = "salmon")) +
  ylim(0,300) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# sysbp, diabp, bmi, heartrate ###################################################
mean_data <- data %>%
  filter(!is.na(BMI)) %>%
  mutate(bmi_group = case_when(
    BMI < 18.5 ~ "Underweight (<18.5)",
    BMI >= 18.5 & BMI < 24.9 ~ "Normal weight (18.5 - 24.9)",
    BMI >= 25 & BMI < 29.9 ~ "Overweight (25.0 - 29.9)",
    BMI >= 30 ~ "Obese (> 30)",
    TRUE ~ NA_character_  # Ensure invalid BMI values result in NA
  )) %>%
  filter(!is.na(bmi_group)) %>%  # Remove any rows where bmi_group is NA
  group_by(bmi_group) %>%
  summarise(
    mean_SysBP = mean(sysBP, na.rm = TRUE),
    mean_DiaBP = mean(diaBP, na.rm = TRUE),
    mean_HeartRate = mean(heartRate, na.rm = TRUE)
  )

long_data <- mean_data %>%
  pivot_longer(cols = c(mean_SysBP, mean_DiaBP, mean_HeartRate), 
               names_to = "Variable", 
               values_to = "MeanValue")

ggplot(long_data, aes(x = bmi_group, y = MeanValue, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +  # Create dodged bars
  labs(title = "Mean of SysBP, DiaBP, and HeartRate by BMI Group",
       x = "BMI Group", y = "Mean Value", fill = "Variable") +
  scale_fill_manual(values = c("mean_SysBP" = "#555B6E", "mean_DiaBP" = "#BEE3DB", "mean_HeartRate" = "#89B0AE")) +
  geom_text(aes(label = round(MeanValue, 2)),  # Round MeanValue to 2 decimal places for the label
            position = position_dodge(width = 1), 
            vjust = -0.5,  
            size = 4, fontface = "bold", family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# DENSITY PLOT
mean_values <- data %>%
  summarise(
    mean_SysBP = mean(sysBP, na.rm = TRUE),
    mean_DiaBP = mean(diaBP, na.rm = TRUE),
    mean_HeartRate = mean(heartRate, na.rm = TRUE)
  )

# Extract mean values from the summary data frame
mean_SysBP <- mean_values$mean_SysBP
mean_DiaBP <- mean_values$mean_DiaBP
mean_HeartRate <- mean_values$mean_HeartRate

ggplot(data) +
  geom_density(aes(x = sysBP, fill = "SysBP"), alpha = 0.5) +  # Density plot for SysBP
  geom_vline(aes(xintercept = mean_SysBP), color = "#E01A4F", linetype = "dashed", size = 1) +  # Mean line for SysBP
  geom_density(aes(x = diaBP, fill = "DiaBP"), alpha = 0.5) +  # Density plot for DiaBP
  geom_vline(aes(xintercept = mean_DiaBP), color = "#53B3CB", linetype = "dashed", size = 1) +  # Mean line for DiaBP
  geom_density(aes(x = heartRate, fill = "HeartRate"), alpha = 0.5) +  # Density plot for HeartRate
  geom_vline(aes(xintercept = mean_HeartRate), color = "#EDAE49", linetype = "dashed", size = 1) +  # Mean line for HeartRate
  labs(title = "Density Plot of SysBP, DiaBP, and HeartRate with Mean Values",
       x = "Value", y = "Density", fill = "Variable") +
  scale_fill_manual(values = c("SysBP" = "#E01A4F", "DiaBP" = "#53B3CB", "HeartRate" = "#F9C22E")) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", color = "#FFFFFA"),
    panel.background = element_rect(fill = "#FAF2F0", color = "#FAF2F0")
  )

# CORRELATION ####################################################################
# HEATMAP
install.packages("reshape2")
library(reshape2)
corr_long <- melt(correlation)
ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title="Correlation Heatmap for Ten Year CHD Risk") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
summary(data)

# STATISTICAL TESTS ##############################################################

# T-Tests #########################################################################
# T-test to compare cholesterol levels between males and females
if ("sex" %in% colnames(data) && "totChol" %in% colnames(data)) {
  cholesterol_sex <- subset(data, !is.na(totChol) & !is.na(sex))
  t_test_cholesterol_sex <- t.test(totChol ~ sex, data = cholesterol_sex)
  print(t_test_cholesterol_sex)
}
# Visualization
if ("sex" %in% colnames(data) && "totChol" %in% colnames(data)) {
  library(ggplot2)
  ggplot(data, aes(x = factor(sex, labels = c("Female", "Male")), y = totChol)) +
    geom_boxplot(fill = c("lightblue", "lightgreen")) +
    labs(x = "Gender", y = "Total Cholesterol", title = "Cholesterol Levels by Gender") +
    theme_minimal()
}

# T-test to compare blood pressure between current smokers and non-smokers
if ("currentSmoker" %in% colnames(data) && "sysBP" %in% colnames(data)) {
  bp_smoker <- subset(data, !is.na(sysBP) & !is.na(currentSmoker))
  t_test_bp_smoker <- t.test(sysBP ~ currentSmoker, data = bp_smoker)
  print(t_test_bp_smoker)
}
# Visualization
if ("currentSmoker" %in% colnames(data) && "sysBP" %in% colnames(data)) {
  ggplot(data, aes(x = factor(currentSmoker, labels = c("Non-Smoker", "Smoker")), y = sysBP)) +
    geom_boxplot(fill = c("lightcoral", "lightyellow")) +
    labs(x = "Smoking Status", y = "Systolic Blood Pressure", title = "Blood Pressure by Smoking Status") +
    theme_minimal()
}

# T-test to compare BMI for those with and without diabetes
if ("BMI" %in% colnames(data) && "diabetes" %in% colnames(data)) {
  bmi_diabetes <- subset(data, !is.na(BMI) & !is.na(diabetes))
  t_test_bmi_diabetes <- t.test(BMI ~ diabetes, data = bmi_diabetes)
  print(t_test_bmi_diabetes)
}
# Visualization
if ("BMI" %in% colnames(data) && "diabetes" %in% colnames(data)) {
  ggplot(data, aes(x = factor(diabetes, labels = c("No Diabetes", "Diabetes")), y = BMI)) +
    geom_boxplot(fill = c("lightpink", "lightcyan")) +
    labs(x = "Diabetes Status", y = "BMI", title = "BMI by Diabetes Status") +
    theme_minimal()
}

# Chi-Square Tests ##################################################################
# Chi-Square test to check if current smoking status is independent of heart disease occurrence
if ("currentSmoker" %in% colnames(data) && "TenYearCHD" %in% colnames(data)) {
  smoker_heart_table <- table(data$currentSmoker, data$TenYearCHD)
  chi_test_smoker_heart <- chisq.test(smoker_heart_table)
  print(chi_test_smoker_heart)
}
# Visualization:
if ("currentSmoker" %in% colnames(data) && "TenYearCHD" %in% colnames(data)) {
  smoker_heart_table <- table(data$currentSmoker, data$TenYearCHD)
  bar_data <- as.data.frame(smoker_heart_table)
  ggplot(bar_data, aes(x = factor(Var1, labels = c("Non-Smoker", "Smoker")), y = Freq, fill = factor(Var2, labels = c("No Heart Disease", "Heart Disease")))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Smoking Status", y = "Frequency", fill = "Heart Disease Status", title = "Smoking Status vs Heart Disease") +
    theme_minimal()
}

# Chi-Square test to check if sex is associated with the presence of hypertension
if ("sex" %in% colnames(data) && "prevalentHyp" %in% colnames(data)) {
  sex_hypertension_table <- table(data$sex, data$prevalentHyp)
  chi_test_sex_hypertension <- chisq.test(sex_hypertension_table)
  print(chi_test_sex_hypertension)
}
# Visualization
if ("sex" %in% colnames(data) && "prevalentHyp" %in% colnames(data)) {
  sex_hypertension_table <- table(data$sex, data$prevalentHyp)
  bar_data <- as.data.frame(sex_hypertension_table)
  ggplot(bar_data, aes(x = factor(Var1, labels = c("Female", "Male")), y = Freq, fill = factor(Var2, labels = c("No Hypertension", "Hypertension")))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Gender", y = "Frequency", fill = "Hypertension Status", title = "Gender vs Hypertension") +
    theme_minimal()
}

# Chi-Square test to check if diabetes is associated with heart disease occurrence
if ("diabetes" %in% colnames(data) && "TenYearCHD" %in% colnames(data)) {
  diabetes_heart_table <- table(data$diabetes, data$TenYearCHD)
  chi_test_diabetes_heart <- chisq.test(diabetes_heart_table)
  print(chi_test_diabetes_heart)
}
# Visualization
if ("diabetes" %in% colnames(data) && "TenYearCHD" %in% colnames(data)) {
  diabetes_heart_table <- table(data$diabetes, data$TenYearCHD)
  bar_data <- as.data.frame(diabetes_heart_table)
  ggplot(bar_data, aes(x = factor(Var1, labels = c("No Diabetes", "Diabetes")), y = Freq, fill = factor(Var2, labels = c("No Heart Disease", "Heart Disease")))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Diabetes Status", y = "Frequency", fill = "Heart Disease Status", title = "Diabetes vs Heart Disease") +
    theme_minimal()
}



