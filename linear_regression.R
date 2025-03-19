library(readr)
library(car)
library(stringr)

heart_disease = read_csv("heart.csv")
heart_disease = as.data.frame(heart_disease)

str(heart_disease)
summary(heart_disease)

# predict Oldpeak from RestingBP, Cholesterol, Age, MaxHR
heart_disease$random_number = runif(nrow(heart_disease), 1, 100)
heart_disease_train = heart_disease[heart_disease[ , "random_number"] <= 80, ]
heart_disease_test = heart_disease[heart_disease[ , "random_number"] > 80, ]

lr_model = lm(Oldpeak ~ RestingBP + Cholesterol + Age + MaxHR, data = heart_disease)
summary(lr_model)
vif(lr_model)

# function to compute measures of accuracy
accuracies = function(model, dataset) {
  predictions = predict(model, newdata = dataset)
  actual_values = dataset$Oldpeak
  
  # filter out rows with a value of 0
  # since number / 0 causes Inf (infinity)
  valid_indices = actual_values != 0
  predictions = predictions[valid_indices]
  actual_values = actual_values[valid_indices]
  residuals = predictions - actual_values
  
  ME = mean(residuals)
  MAE = mean(abs(residuals))
  MPE = mean(residuals / actual_values) * 100
  MAPE = mean(abs(residuals / actual_values)) * 100
  RMSE = sqrt(mean(residuals^2))
  
  # Reference: https://stringr.tidyverse.org/reference/str_interp.html
  cat(str_interp("Mean Error is $[.2f]{ME}\n"))
  cat(str_interp("Mean Absolute Error is $[.2f]{MAE}\n"))
  cat(str_interp("Mean Percent Error is $[.2f]{MPE}%\n"))
  cat(str_interp("Mean Absolute Percent Error is $[.2f]{MAPE}%\n"))
  cat(str_interp("Root Mean Squared Error is $[.2f]{RMSE}\n"))
}

accuracies(lr_model, heart_disease_test)

# Mean Error of -0.63 means that on average, the predictions are slightly underestimating
# the actual values by 0.63 units.

# Mean Absolute Error means that regardless of sign, our predictions are off from the
# actual values by 0.89 units.

# Mean Percent Error means that on average, our predictions are underestimating the
# actual values by about 11.71%.

# Mean Absolute Percent Error means that regardless of sign, our predictions are off
# from the actual values by 68.18%. MAPE is sensitive to small values in the denominator
# (in this case, the actual values), which is why the percentage seems high.

# Root Mean Squared Error gives us the standard deviation of the residuals. It means that
# our predictions are off from the actual values by 1.20 units.
