library(readr)
library(C50)
library(stringr)
library("rpart")
library("rpart.plot") 

heart_disease = read_csv("heart.csv")
heart_disease = as.data.frame(heart_disease)

# convert all columns to a factor
columns = list("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG", "MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease")
for (col_name in columns) {
  heart_disease[[col_name]] = as.factor(heart_disease[[col_name]])
}
str(heart_disease)

# create 80% train set and 20% test set
heart_disease$random_number = runif(nrow(heart_disease), min=1, max=100)
heart_disease_train = heart_disease[heart_disease[ , "random_number"] <= 80, ]
heart_disease_test = heart_disease[heart_disease[ , "random_number"] > 80, ]

# C5.0 full model
full_model_C50 = C5.0(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = heart_disease_train)
print(full_model_C50)
summary(full_model_C50)
plot(full_model_C50)

# function to compute the accuracy score and confusion matrix for a given model
accuracies = function(model, test_data, model_name, type=NULL) {
  if (is.null(type)) {
    predictions = predict(model, test_data)
  }
  else {
    predictions = predict(model, test_data, type=type)
  }
  actual_values = test_data$HeartDisease
  cat(str_interp("Accuracy measures for ${model_name}:\n"))
  cat("Confusion matrix:\n")
  cm = table(predictions, actual_values)
  print(cm)
  accuracy = sum(diag(cm)) / sum(cm)
  cat(str_interp("Accuracy score is $[.2f]{accuracy * 100}%\n"))
}

accuracies(full_model_C50, heart_disease_test, "Full Model C5.0")


# CI Full Model
# CI_full_model = ctree(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data=heart_disease_train)
CI_full_model = ctree(HeartDisease ~ Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + ExerciseAngina + ST_Slope, data=heart_disease_train)
print(CI_full_model)
plot(CI_full_model)

accuracies(CI_full_model, heart_disease_test, "Full Model CI")
# with all of the additional nodes in CI model, our accuracy score only increases by about 7% compared to
# C5.0's accuracy score. This indicates that ST_Slope is a strong indicator of heart disease.


# CART Full Model
full_model_cart = rpart(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = heart_disease_train, method="class")
print(full_model_cart)
summary(full_model_cart)

# plot the model
rpart.plot(full_model_cart, main="Predicting Heart Disease - Full CART Classification Tree", box.palette="Blues")

accuracies(full_model_cart, heart_disease_test, "Full Model CART", type="class")

# CART Small Model
small_model_cart = rpart(HeartDisease ~ Cholesterol + MaxHR + ST_Slope + Oldpeak, data = heart_disease_train, method="class")
print(small_model_cart)
summary(small_model_cart)
rpart.plot(small_model_cart, main="Predicting Heart Disease - Small CART Classification Tree", box.palette="Blues")

accuracies(small_model_cart, heart_disease_test, "Small Model CART", type="class")
