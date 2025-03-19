library(readr)
library(corrplot)
library(FactoMineR)
library(factoextra)
source("https://blogs.5eanalytics.com/RCode/PCA_functions.R")
library(car)
library(psych)

heart_failure_data = read_csv("heart.csv")
heart_failure_data = as.data.frame(heart_failure_data)

# check for null values
colSums(is.na(heart_failure_data))
# here we can see that there are no nulls

# structure and summary statistics
str(heart_failure_data)
summary(heart_failure_data)

# correlation analysis for numerical features
numeric_columns_only = heart_failure_data[ , c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak", "HeartDisease")]
correlation_matrix = cor(numeric_columns_only)
print(correlation_matrix)

# correlation chart
corrplot(correlation_matrix,
         method="color",
         type="lower",
         diag=FALSE, # get rid of diagonal where correlation is 1 for every entry
         # order="alphabet", # order the plot alphabetically
         addCoef.col="black", # set coefficient text to black
         number.cex=0.8, # size of coefficient text
         tl.col="black", # text label color
         tl.srt=50 # text label direction
)


# hypothesis tests for categorical variables

# chi-squared test for association between ChestPainType and HeartDisease
# H0 - There is no significant association between ChestPainType and HeartDisease
# Ha - There is a significant association between ChestPainType and HeartDisease
contingency_table = table(heart_failure_data$ChestPainType, heart_failure_data$HeartDisease)
print(contingency_table)
result = chisq.test(contingency_table)
print(result)

# p-value is < 2.2e-16, so we reject the null hypothesis and conclude that there IS a
# significant association between ChestPainType and HeartDisease

# From the contingency table, we can see that individuals with ASY (asymptomatic chest pain)
# are much more likely to have heart disease than other categories of chest pain.
# ASY = 392 / (392 + 104) = 79.03%
# ATA = 24  / (24 + 149)  = 13.87%
# NAP = 72  / (72 + 131)  = 35.47%
# TA  = 20  / (20 + 26)   = 43.48%


# chi-squared test for association between ST_Slope and HeartDisease
# H0 - There is no association between slope of peak exercise and heart disease.
# Ha - There is a significant association between slope of peak exercise and heart disease.
contingency_table = table(heart_failure_data$ST_Slope, heart_failure_data$HeartDisease)
print(contingency_table)
result = chisq.test(contingency_table)
print(result)

# p-value is < 2.2e-16, so we reject the null hypothesis and conclude that there IS a
# significant association between slope of peak exercise and heart disease.

# From the contigency table, we can see that individuals with a Down or Flat st slope
# are more likely to have heart disease than someone with an Up st slope.
# Down = 49  / (14 + 49)  = 77.78%
# Flat = 381 / (79 + 381) = 82.83%
# Up   = 78  / (317 + 78) = 19.75%


# chi-squared test for association between RestingECG and HeartDisease
# H0: There is no assocation between Resting ECG and heart disease.
# Ha: There is an association between Resting ECG and heart disease.
contingency_table = table(heart_failure_data$RestingECG, heart_failure_data$HeartDisease)
print(contingency_table)
result = chisq.test(contingency_table)
print(result)

# p-value = 0.004229 suggests that there is strong evidence to reject H0,
# therefore, we conclude that there is an association between the type of
# Resting ECG and heart disease.

# From contingency table, we can calculate the following:
# LVH    = 106 / (82 + 106)  = 56.38%
# Normal = 285 / (267 + 285) = 51.63%
# ST     = 117 / (61 + 117)  = 65.73%

# So, individuals with ST-T wave abnormality are most likely to have a heart disease.
# This is then followed by left ventricular hypertrophy, and then normal ECG results.
# However, the association between the two variables is not as strong when compared to
# the variables in the previous chi-squared tests (ChestPainType and ST_Slope). This is 
# confirmed by an X-squared value of 10.931 (much smaller than the previous two tests).


# PCA Analysis
# convert all categorical columns to numeric
copy = heart_failure_data

copy$Sex = as.numeric(factor(copy$Sex, levels = c("M", "F"))) - 1
copy$ChestPainType = as.numeric(factor(copy$ChestPainType, levels = c("TA", "ATA", "NAP", "ASY")))
copy$RestingECG = as.numeric(factor(copy$RestingECG, levels = c("Normal", "ST", "LVH"))) - 1
copy$ExerciseAngina = as.numeric(factor(copy$ExerciseAngina, levels = c("N", "Y"))) - 1
copy$ST_Slope = as.numeric(factor(copy$ST_Slope, levels = c("Down", "Flat", "Up"))) - 1

str(copy)

pca_result = PCA(X=copy, scale.unit=T, graph=T)
summary(pca_result)

# screeplot criterion
fviz_screeplot(pca_result, ncp=10)

# minimal communality criterion
communality(pca_result)

# PCA interpretation
display_pc(pca_result)


# Factor Analysis
# perform a hypothesis test on the data
# H0: correlations are zero (all independent)
# Ha: correlations are not zero (variables are dependent/correlated)
# We want this test to reject H0 and accept Ha
# - this way, we know that the variables are correlated,
# - and we can group them together into factors
# - if none of the variables are correlated, then there is no use in 
# - doing factor analysis because we can't group items together
copy = copy[ , -c(12)]
cormat = cor(copy)
cormat = round(cormat, 3)
cortest.bartlett(cormat, n=nrow(copy))
# here, we can see that p-value = 7.7e-294, so there is definitely correlation
# between the variables

f = factanal(copy, factors=5, rotation="promax")
print(f, cutoff=.249)
# f1 = chest pain, angina
# f2 = oldpeak, st slope
# f3 = max hr
# f4 = sex, cholesterol
# f5 = age, resting bp, fasting bs, resting ecg
# p-value = 0.0171 < 0.05

# f = factanal(copy, factors=6, rotation="promax")
# print(f, cutoff=.27)
# f1 = chest pain, angina
# f2 = oldpeak, st slope
# f3 = max hr
# f4 = sex, cholesterol, fasting bs
# f5 = resting bp
# f6 = age, resting ecg
# p-value = 0.163 > 0.05
