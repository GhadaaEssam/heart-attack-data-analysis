# Install required packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("factoextra")

# Load libraries
library(factoextra)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)

# Load dataset
dataset <- read.csv("C:/Users/dell/OneDrive/archive/heart.csv")

# Data cleaning
# Check for duplicates
sum(duplicated(dataset))

# Remove duplicates
data <- distinct(dataset)

# Check for missing values
sum(is.na(data))

# Check data structure
all_columns_numeric <- sapply(data, function(col) all(sapply(col, is, "numeric")))

# Subset data for patients and non-patients
patients <- subset(data, output == 1)
non_patients <- subset(data, output == 0)

# Age analysis
age_summary <- summary(data$age)
age_outlier <- boxplot(data$age, main = "Boxplot of Age")$out
sum(age_outlier)  # No outliers in age

# Sex analysis
sex_counts <- table(data$sex)
percentages <- prop.table(sex_counts) * 100
pie(percentages, labels = paste(c('F','M'), ": ", round(percentages, 1), "%"),
    col = c("lightpink", "lightblue"), main = "Gender Distribution")

# Patients sex distribution
patients_sex_counts <- table(patients$sex)
patients_percentages <- prop.table(patients_sex_counts) * 100
pie(patients_percentages, labels = paste(c('F','M'), ": ", round(patients_percentages, 1), "%"),
    col = c("lightpink", "lightblue"), main = "Gender Distribution - Heart attack")

# Split by sex
male <- subset(data, sex == 1)
female <- subset(data, sex == 0)

# Male heart attack distribution
male_output_counts <- table(male$output)
male_percentages <- prop.table(male_output_counts) * 100
pie(male_percentages, labels = paste(c('non-patient','patient'), ": ", round(male_percentages, 1), "%"),
    col = c("lightgreen", "lightpink"), main = "male percentages")

# Female heart attack distribution
female_output_counts <- table(female$output)
female_percentages <- prop.table(female_output_counts) * 100
pie(female_percentages, labels = paste(c('non-patient','patient'), ": ", round(female_percentages, 1), "%"),
    col = c("lightgreen", "lightpink"), main = "female percentages")

# Non-patients sex distribution
non_patients_sex_counts <- table(non_patients$sex)
non_patients_percentages <- prop.table(non_patients_sex_counts) * 100
pie(non_patients_percentages, labels = paste(c('F','M'), ": ", round(non_patients_percentages, 1), "%"),
    col = c("lightpink", "lightblue"), main = "Gender Distribution - No Heart attack")

# Chest pain (cp) analysis
cp_category <- c("Typical Angina","Atypical Angina","Non-anginal pain","Asymptomatic")
cp_values <- c(sum(data$cp == 0), sum(data$cp == 1), sum(data$cp == 2), sum(data$cp == 3))
cp_df <- data.frame(category = cp_category, values = cp_values)

ggplot(cp_df, aes(x = cp_category, y = cp_values, fill = cp_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Chest Pain Distribution", x = "Chest Pain Type", y = "Values")

# Patients chest pain
patients_cp_category <- c("Typical Angina","Atypical Angina","Non-anginal pain","Asymptomatic")
patients_cp_values <- c(sum(data$cp == 0), sum(patients$cp == 1), sum(patients$cp == 2), sum(patients$cp == 3))
patients_cp_df <- data.frame(category = patients_cp_category, values = patients_cp_values)

ggplot(patients_cp_df, aes(x = patients_cp_category, y = patients_cp_values, fill = patients_cp_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Chest Pain Distribution for patients", x = "Chest Pain Type", y = "Values")

# Resting blood pressure (trtbps)
trtbps_summary <- summary(data$trtbps)
trtbps_outlier <- boxplot(data$trtbps, main = "Chest Pain Distribution")$out

hist(data$trtbps[data$output == 0], col = "purple", alpha = 0.7, main = "Distribution of Blood Pressure - No Heart Attack Status")
hist(data$trtbps[data$output == 1], col = "skyblue", alpha = 0.7, main = "Distribution of Blood Pressure - Heart Attack Status")

# Cholesterol (chol)
chol_summary <- summary(data$chol)
chol_outlier <- boxplot(data$chol, main = "Chest Pain Distribution")$out
hist(data$chol[data$output == 0], col = "limegreen", alpha = 0.7, main = "Cholesterol Distribution - No Heart Attack")
hist(data$chol[data$output == 1], col = "maroon", alpha = 0.7, main = "Cholesterol Distribution - Heart Attack")

# Fasting blood sugar (fbs)
fbs_count <- table(data$fbs)
fb_percentage <- prop.table(fbs_count) * 100
pie(fb_percentage, labels = paste(round(fb_percentage, 1), "%"),
    col = c("darkgrey", "magenta"), main = "Fasting Blood Sugar Distribution")
legend("bottomright", legend = c("FBS > 120 mg/dl", "FBS < 120 mg/dl"), fill = c("darkgrey", "magenta"))

# Patients fasting blood sugar
fbs_count <- table(patients$fbs)
fb_percentage <- prop.table(fbs_count) * 100
pie(fb_percentage, labels = paste(round(fb_percentage, 1), "%"),
    col = c("darkgrey", "magenta"), main = "patients Fasting Blood Sugar Distribution")
legend("bottomright", legend = c("FBS > 120 mg/dl", "FBS < 120 mg/dl"), fill = c("darkgrey", "magenta"))

# Resting electrocardiographic results (restecg)
rstecg_category <- c("Normal","ST-T wave abnormality","left ventricular hypertrophy")
rstecg_values <- c(sum(data$restecg == 0), sum(data$restecg == 1), sum(data$restecg == 2))
rstecg_df <- data.frame(category = rstecg_category, values = rstecg_values)

ggplot(rstecg_df, aes(x = rstecg_category, y = rstecg_values, fill = rstecg_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Resting Electrocardiographic Results", x = "Results", y = "Values")

# Maximum heart rate (thalachh)
thalachh_summary <- summary(data$thalachh)
thalachh_outlier <- boxplot(data$thalachh, main = "Maximum Heart Rate Distribution")$out

boxplot(data$thalachh, main = "Distribution of Maximum Heart Rate")
boxplot(data$thalachh ~ data$output, data = data, main = "Maximum Heart Rate by Heart Attack Status")

hist(data$thalachh[data$output == 0], col = "magenta", alpha = 0.7, main = "Maximum Heart Rate Distribution - NO Heart Attack Status")
hist(data$thalachh[data$output == 1], col = "gold", alpha = 0.7, main = "Maximum Heart Rate Distribution - Heart Attack Status")

ggplot(data, aes(x = seq_along(thalachh), y = thalachh)) +
  geom_point() +
  labs(title = "Scatterplot of Maximum Heart Rate", x = "Observation", y = "Maximum Heart Rate")

# Exercise induced angina (exng)
exng_count <- table(data$exng)
exng_percentage <- prop.table(exng_count) * 100
pie(exng_percentage, labels = paste(round(exng_percentage, 1), "%"),
    col = c("darkgrey", "magenta"), main = "Exercise Induced Angina")
legend("bottomright", legend = c("No", "Yes"), fill = c("darkgrey", "magenta"))

# Oldpeak
hist(data$oldpeak[data$output == 0], col = "gold", alpha = 0.7, main = "Oldpeak Observations - No Heart Attack")
hist(data$oldpeak[data$output == 1], col = "gold", alpha = 0.7, main = "Oldpeak Observations - Heart Attack")

ggplot(data, aes(x = seq_along(oldpeak), y = oldpeak)) +
  geom_point() +
  labs(title = "Scatterplot of Oldpeak Values", x = "Observation", y = "Oldpeak")

# Slope of peak exercise ST segment (slp)
slp_category <- c("Unsloping","Downsloping","Flat")
slp_values <- c(sum(data$slp == 0), sum(data$slp == 1), sum(data$slp == 2))
slp_df <- data.frame(category = slp_category, values = slp_values)

ggplot(slp_df, aes(x = slp_category, y = slp_values, fill = slp_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "The slope of the peak exercise ST segment", x = "Observations", y = "Values")

# Thalassemia (thall)
thall_category <- c("Fixed Defect","Normal Blood","Reversible Defect")
thall_values <- c(sum(data$thall == 0), sum(data$thall == 1), sum(data$thall == 2))
thall_df <- data.frame(category = thall_category, values = thall_values)

ggplot(thall_df, aes(x = thall_category, y = thall_values, fill = thall_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Thalassemia/Blood flow", x = "Observations", y = "Values")

# Number of major vessels (caa)
caa_count <- table(data$caa)
caa_percentage <- prop.table(caa_count) * 100
pie(caa_percentage, labels = paste(round(caa_percentage, 1), "%"),
    col = c("darkgrey", "magenta","orange","maroon","yellow"), main = "Number of Major Vessels")
legend("bottomright", legend = c("0", "2","1","3","4"), fill = c("darkgrey", "magenta","orange","maroon","yellow"))

# Decision Tree
tree_model <- rpart(output ~ age + sex + cp + trtbps + chol + fbs + restecg + thalachh + exng + oldpeak + slp + thall + caa, 
                    data = data, method = "class")
rpart.plot(tree_model)
print(tree_model)
printcp(tree_model)

# Prediction with new data
new_data <- data.frame(
  age = 55,
  sex = 1,
  cp = 1,
  trtbps = 120,
  chol = 180,
  fbs = 0,
  restecg = 0,
  thalachh = 160,
  exng = 0,
  oldpeak = 1.53,
  slp = 1,
  thall = 2,
  caa = 0
)
predictions <- predict(tree_model, newdata = new_data, type = "class")
print(predictions)

# K-means Clustering
scaled_data <- scale(data)
kmeans <- kmeans(scaled_data, centers = 3)
fviz_cluster(kmeans, data = scaled_data, geom = 'point', ggtheme = theme_bw())