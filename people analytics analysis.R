rm(list=ls())
hr_data<-read.csv("hr_data.csv")
str(hr_data)

##checking for missing values
colSums(is.na(hr_data))
#There are no missing values in the data

#summary statistics
summary(hr_data$Age)


# Load the required libraries
library(dplyr)
library(caTools)
library(dplyr)
library(caret)
library(ROSE)

# Handling outliers
# Replace outliers with NA or use a specific outlier detection method
# Define upper and lower thresholds for outlier handling
upper_threshold <- 60 
lower_threshold <- 20
hr_data$outliers <- ifelse(hr_data$Age > upper_threshold | hr_data$Age < lower_threshold, NA, hr_data$Age)

# Feature selection
# Use a suitable feature selection method like correlation, chi-square, or recursive feature elimination
selected_features <- caret::nearZeroVar(hr_data)

# Dummy encoding
# Identify categorical variables and perform dummy encoding
categorical_vars <- c("DOJ.Extended", "Offered.band", "Joining.Bonus", "Candidate.relocate.actual","Gender","Candidate.Source","Status")
encoded_data <- dummyVars(~., data = hr_data[, categorical_vars])
hr_data_encoded <- data.frame(predict(encoded_data, newdata = hr_data))

# Noise reduction
# Apply noise reduction techniques like smoothing, filtering, or outlier removal
hr_data_noise_reduced <- hr_data_encoded %>%
  dplyr::filter(hr_data$DOJ.Extended > lower_threshold)


### Logistics Regression

# Select relevant variables for logistic regression
selected_vars <- c("DOJ.Extended", "Offered.band", "Joining.Bonus", "Candidate.relocate.actual","Gender","Candidate.Source","Status","Age","Duration.to.accept.offer","Notice.period","Pecent.hike.expected.in.CTC","Percent.hike.offered.in.CTC","Percent.difference.CTC","Rex.in.Yrs")
hr_data <- hr_data[selected_vars]

# Convert the 'status' variable to a factor
hr_data$Status <- as.factor(hr_data$Status)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
split <- sample.split(hr_data$Status, SplitRatio = 0.7)
train_data <- subset(hr_data, split == TRUE)
test_data <- subset(hr_data, split == FALSE)

# Perform logistic regression
model <- glm(Status ~ ., data = train_data, family = binomial)
summary(model)

# Predict the status using the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to classes
predicted_classes <- ifelse(predictions > 0.5, "joined", "not joined")

# Evaluate the model
confusion_matrix <- table(predicted_classes, test_data$Status)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Display the confusion matrix and accuracy
print(confusion_matrix)
cat("Accuracy:", accuracy)

# Extract the values from the confusion matrix
tp <- confusion_matrix[1, 1]  # True Positives
tn <- confusion_matrix[2, 2]  # True Negatives
fp <- confusion_matrix[2, 1]  # False Positives
fn <- confusion_matrix[1, 2]  # False Negatives

# Calculate sensitivity (recall)
sensitivity <- tp / (tp + fn)

# Calculate specificity
specificity <- tn / (tn + fp)

# Calculate precision
precision <- tp / (tp + fp)

# Calculate F1 score
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Print the results
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")



###Random Forest

# Load the required libraries
library(randomForest)

# Perform random forest classification
rf_model <- randomForest(Status ~ ., data = train_data, ntree = 100)

# Predict the status using the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
rf_confusion_matrix <- confusionMatrix(rf_predictions, test_data$Status)

# Display the confusion matrix 
print(rf_confusion_matrix)

###calculate precision and other metrics
rf_cf <- table(rf_predictions, test_data$Status)


# Extract the values from the confusion matrix
rf_tp <- rf_cf[1, 1]  # True Positives
rf_tn <- rf_cf[2, 2]  # True Negatives
rf_fp <- rf_cf[2, 1]  # False Positives
rf_fn <- rf_cf[1, 2]  # False Negatives

# Calculate sensitivity (recall)
rf_sensitivity <- rf_tp / (rf_tp + rf_fn)

# Calculate specificity
rf_specificity <- rf_tn / (rf_tn + rf_fp)

# Calculate precision
rf_precision <- rf_tp / (rf_tp + rf_fp)

# Calculate F1 score
rf_f1_score <- 2 * (rf_precision * rf_sensitivity) / (rf_precision + rf_sensitivity)

# Print the results
cat("Sensitivity:", rf_sensitivity, "\n")
cat("Specificity:", rf_specificity, "\n")
cat("Precision:", rf_precision, "\n")
cat("F1 Score:", rf_f1_score, "\n")



### K Nearest Neighbors

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

set.seed(7)
fit.knn <- train(Status~., data=train_data, method="knn",
                 metric=metric ,trControl=trainControl)
knn.k1 <- fit.knn$bestTune # keep this Initial k for testing with knn() function in next section
print(fit.knn)

plot(fit.knn)

set.seed(7)
knn_prediction <- predict(fit.knn, newdata = test_data)
knn_confusion_matrix <- confusionMatrix(knn_prediction, test_data$Status)

# Display the confusion matrix 
print(knn_confusion_matrix)


###calculate precision and other metrics
knn_cf <- table(knn_prediction, test_data$Status)


# Extract the values from the confusion matrix
knn_tp <- knn_cf[1, 1]  # True Positives
knn_tn <- knn_cf[2, 2]  # True Negatives
knn_fp <- knn_cf[2, 1]  # False Positives
knn_fn <- knn_cf[1, 2]  # False Negatives

# Calculate sensitivity (recall)
knn_sensitivity <- knn_tp / (knn_tp + knn_fn)

# Calculate specificity
knn_specificity <- knn_tn / (knn_tn + knn_fp)

# Calculate precision
knn_precision <- knn_tp / (knn_tp + knn_fp)

# Calculate F1 score
knn_f1_score <- 2 * (knn_precision * knn_sensitivity) / (knn_precision + knn_sensitivity)

# Print the results
cat("Sensitivity:", knn_sensitivity, "\n")
cat("Specificity:", knn_specificity, "\n")
cat("Precision:", knn_precision, "\n")
cat("F1 Score:", knn_f1_score, "\n")





###Decision Trees

# Load the required libraries
library(rpart)
library(rpart.plot)

# Build the decision tree
dt_model <- rpart(Status ~ ., data = train_data, method = "class")

# Plot the decision tree
rpart.plot(dt_model)

# Predict the status using the test data
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")

# Evaluate the model
dt_confusion_matrix<-confusionMatrix(dt_predictions, test_data$Status)

# Display the confusion matrix and accuracy
print(dt_confusion_matrix)


###calculate precision and other metrics
dt_cf <- table(dt_predictions, test_data$Status)


# Extract the values from the confusion matrix
dt_tp <- dt_cf[1, 1]  # True Positives
dt_tn <- dt_cf[2, 2]  # True Negatives
dt_fp <- dt_cf[2, 1]  # False Positives
dt_fn <- dt_cf[1, 2]  # False Negatives

# Calculate sensitivity (recall)
dt_sensitivity <- dt_tp / (dt_tp + dt_fn)

# Calculate specificity
dt_specificity <- dt_tn / (dt_tn + dt_fp)

# Calculate precision
dt_precision <- dt_tp / (dt_tp + dt_fp)

# Calculate F1 score
dt_f1_score <- 2 * (dt_precision * dt_sensitivity) / (dt_precision + dt_sensitivity)

# Print the results
cat("Sensitivity:", dt_sensitivity, "\n")
cat("Specificity:", dt_specificity, "\n")
cat("Precision:", dt_precision, "\n")
cat("F1 Score:", dt_f1_score, "\n")


####
#Plot ROC curves
# Load necessary libraries
library(pROC)
true_labels <- test_data$Status

# Calculate predicted probabilities for each model
logistic_probs <- predict(model, newdata = test_data, type = "response")
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]  # Assuming binary classification
knn_probs <- predict(fit.knn, newdata = test_data,type = "prob")[, 2]  # Assuming binary classification
dt_probs <- predict(dt_model, newdata = test_data, type = "prob")[, 2]  # Assuming binary classification

# Create a data frame with predicted probabilities and true labels
roc_data <- data.frame(
  Logistic = logistic_probs,
  Random_Forest = rf_probs,
  KNN = knn_probs,
  Decision_Tree = dt_probs,
  True_Labels = true_labels
)

# Create a list to store the ROC curves
roc_curves <- list()

# Calculate ROC curve for each model and store in the list
roc_curves$Logistic <- roc(roc_data$True_Labels, roc_data$Logistic)
roc_curves$Random_Forest <- roc(roc_data$True_Labels, roc_data$Random_Forest)
roc_curves$KNN <- roc(roc_data$True_Labels, roc_data$KNN)
roc_curves$Decision_Tree <- roc(roc_data$True_Labels, roc_data$Decision_Tree)

# Plot the ROC curves
plot(roc_curves$Logistic, col = "blue", main = "ROC Curves", lwd = 2)
plot(roc_curves$Random_Forest, col = "red", add = TRUE, lwd = 2)
plot(roc_curves$KNN, col = "green", add = TRUE, lwd = 2)
plot(roc_curves$Decision_Tree, col = "purple", add = TRUE, lwd = 2)

# Add legend
legend("bottomright", legend = c("Logistic", "Random Forest", "KNN", "Decision Tree"), col = c("blue", "red", "green", "purple"), lwd = 2)

# Add AUC values
text(0.6, 0.4, paste("AUC (Logistic):", round(auc(roc_curves$Logistic), 3)), col = "blue")
text(0.6, 0.35, paste("AUC (Random Forest):", round(auc(roc_curves$Random_Forest), 3)), col = "red")
text(0.6, 0.3, paste("AUC (KNN):", round(auc(roc_curves$KNN), 3)), col = "green")
text(0.6, 0.25, paste("AUC (Decision Tree):", round(auc(roc_curves$Decision_Tree), 3)), col = "purple")


# Extract variable importance
logistic_importance <- varImp(model)

# Print variable importance
print(logistic_importance)

#2. Random forest

# Calculate variable importance
rf_importance <- importance(rf_model)

# Print variable importance
print(rf_importance)






