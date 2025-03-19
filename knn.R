library(readr)
library(class)
library(stringr)

heart_disease = read_csv("heart.csv")
heart_disease = as.data.frame(heart_disease)

# convert target variable to a factor
heart_disease$HeartDisease = as.factor(heart_disease$HeartDisease)
str(heart_disease)

# convert all other columns to numeric if they aren't already
columns = list("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")
for (col_name in columns) {
  heart_disease[[col_name]] = as.numeric(as.factor(heart_disease[[col_name]]))
}
str(heart_disease)

# create 80% train set and 20% test set
heart_disease$random_number = runif(nrow(heart_disease), min=1, max=100)
heart_disease_train = heart_disease[heart_disease[ , "random_number"] <= 80, ]
heart_disease_test = heart_disease[heart_disease[ , "random_number"] > 80, ]

# find best k value
# list of k values to test
# Reference: https://stackoverflow.com/questions/11454333/r-numbers-from-1-to-100
k_values = 1:21
# Reference: https://www.geeksforgeeks.org/how-to-create-a-vector-of-specific-type-and-length-in-r/
accuracies = vector("numeric", 21)

for (k in k_values) {
  knn_pred = knn(heart_disease_train, heart_disease_test, heart_disease_train$HeartDisease, k=k)
  
  # calculate accuracy via confusion matrix
  knn_cm = table(knn_pred, heart_disease_test$HeartDisease)
  knn_accuracy = sum(diag(knn_cm)) / sum(knn_cm)
  accuracies[k] = knn_accuracy
}

# find the best k value (highest accuracy)
# Reference: https://stackoverflow.com/questions/743622/finding-row-index-containing-maximum-value-using-r
best_k = which.max(accuracies)
cat(str_interp("Best k value: ${best_k}\n"))

# Reference: https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/plot
plot(k_values, accuracies, type='b', main='k-value vs. Accuracy', xlab='k-value', ylab='Accuracy')


# create best model
best_knnmdl = knn(heart_disease_train, heart_disease_test, heart_disease_train$HeartDisease, k=best_k, prob=TRUE)
# print(best_knnmdl)

# confusion matrix and accuracy
knn_actual = heart_disease_test$HeartDisease
knn_cm = table(best_knnmdl, knn_actual)
print(knn_cm)
knn_accuracy = sum(diag(knn_cm)) / sum(knn_cm)
cat(str_interp("Accuracy score is $[.2f]{knn_accuracy * 100}%\n"))
