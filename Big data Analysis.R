
df<-read.csv("advertising.csv")
str(df)
colSums(is.na(df))

##Summary statistics
library(psych)
describe(df)

#outlier detection

boxplot(df[,c("Daily.Time.Spent.on.Site","Age","Area.Income","Daily.Internet.Usage")],col = c('blue','yellow','red','purple'),main='Outlier Detection',ylim=c(-10000,80000),ylab='count')

##outlier removal
area_income <- df$Area.Income

# Calculate the interquartile range (IQR)
IQR <- IQR(area_income)

# Calculate the upper and lower bounds for outliers
upper_bound <- quantile(area_income, 0.75) + 1.5 * IQR
lower_bound <- quantile(area_income, 0.25) - 1.5 * IQR

df <- df %>%
  filter(area_income >= lower_bound, area_income <= upper_bound)

#correlation
cor(df[,c("Daily.Time.Spent.on.Site","Age","Area.Income","Daily.Internet.Usage")])

##Distribution of clicking ad
df$Clicked.on.Ad<-as.factor(df$Clicked.on.Ad)
count1<-table(df$Clicked.on.Ad)
barplot(count1, main = "distribution of clicking ad", col = "Navy",xlab = "clicking ad(1=Yes/0=No)", ylab = "Frequency")

df$Male<-as.factor(df$Male)
count2<-table(df$Male)
barplot(count2,main = "Distribution of gender",col = "orange", xlab = "gender(1=Male/0=female)", ylab = "Frequency")

library(ggplot2)
##Distribution of age
ggplot(df, aes(x=Age)) + geom_density(col="blue") + 
  geom_histogram(aes(y=..density..), colour="black", fill=NA)

#Distribution of area income
ggplot(df, aes(x=Area.Income)) + geom_density(col="blue") + 
  geom_histogram(aes(y=..density..), colour="black", fill=NA)

#Distribution of daily internet usage
ggplot(df, aes(x=Daily.Internet.Usage)) + geom_density(col="blue") + 
  geom_histogram(aes(y=..density..), colour="black", fill=NA)

#Distribution of time spent on internet
ggplot(df, aes(x=Daily.Time.Spent.on.Site)) + geom_density(col="blue") + 
  geom_histogram(aes(y=..density..), colour="black", fill=NA)

#Distribution of age and clicked on ad
ggplot(df, aes(x=Age, fill=Clicked.on.Ad)) + geom_histogram() + facet_wrap(~Clicked.on.Ad)

##Distribution of Age, Daily time spent on site and clicked on ad
ggplot(df, aes(x=Daily.Time.Spent.on.Site, y=Age, fill=Clicked.on.Ad)) + geom_boxplot()
+ facet_wrap(~Male, ncol=2)


#make this example reproducible
set.seed(123)

#Use 70% of dataset as train_dataing set and remaining 30% as test_dataing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train_data  <- df[sample, ]
test_data   <- df[!sample, ]

train_data$Clicked.on.Ad<-as.factor(train_data$Clicked.on.Ad)
test_data$Clicked.on.Ad<-as.factor(test_data$Clicked.on.Ad)

##Logistics regression
model <- glm(Clicked.on.Ad ~ Age + Daily.Time.Spent.on.Site + Area.Income +
               Daily.Internet.Usage, data = train_data, family = "binomial")
summary(model)

##Prediction
library(tidyverse)
probabilities <- model %>% predict(test_data, type = "response")
head(probabilities)

contrasts(test_data$Clicked.on.Ad)

predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
head(predicted.classes)

#Accuracy of the model
mean(predicted.classes == test_data$Clicked.on.Ad)


#Confusion matrix
threshold=0.5
predicted_values<-ifelse(predict(model,type="response")>threshold,1,0)
actual_values<-model$y
confusion_matrix<-table(predicted_values,actual_values)
confusion_matrix


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




##Random Forest

# Load the required libraries
library(randomForest)

# Perform random forest classification
rf_model <- randomForest(Clicked.on.Ad ~ Age + Daily.Time.Spent.on.Site + Area.Income +
                           Daily.Internet.Usage, data = train_data, ntree = 100)

# Predict the status using the test_data data
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
rf_confusion_matrix <- confusionMatrix(rf_predictions, test_data$Clicked.on.Ad)

# Display the confusion matrix 
print(rf_confusion_matrix)

###calculate precision and other metrics
rf_cf <- table(rf_predictions, test_data$Clicked.on.Ad)


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
library(caret)

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

set.seed(7)
fit.knn <- train(Clicked.on.Ad ~ Age + Daily.Time.Spent.on.Site + Area.Income +
                        Daily.Internet.Usage, data=train_data, method="knn",
                 metric=metric ,trControl=trainControl)
knn.k1 <- fit.knn$bestTune # keep this Initial k for test_dataing with knn() function in next section
print(fit.knn)

plot(fit.knn)

set.seed(7)
knn_prediction <- predict(fit.knn, newdata = test_data)
knn_confusion_matrix <- confusionMatrix(knn_prediction, test_data$Clicked.on.Ad)

# Display the confusion matrix 
print(knn_confusion_matrix)


###calculate precision and other metrics
knn_cf <- table(knn_prediction, test_data$Clicked.on.Ad)


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
dt_model <- rpart(Clicked.on.Ad ~ Age + Daily.Time.Spent.on.Site + Area.Income +
                    Daily.Internet.Usage, data = train_data, method = "class")

# Plot the decision tree
rpart.plot(dt_model)

# Predict the status using the test_data data
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")

# Evaluate the model
dt_confusion_matrix<-confusionMatrix(dt_predictions, test_data$Clicked.on.Ad)

# Display the confusion matrix and accuracy
print(dt_confusion_matrix)


###calculate precision and other metrics
dt_cf <- table(dt_predictions, test_data$Clicked.on.Ad)


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
true_labels <- test_data$Clicked.on.Ad

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


###Feature selection/ Variable importance
#1. Logistics regression

# Extract variable importance
logistic_importance <- varImp(model)

# Print variable importance
print(logistic_importance)

#2. Random forest

# Calculate variable importance
rf_importance <- importance(rf_model)

# Print variable importance
print(rf_importance)

#3. KNN 

# Extract variable importance
knn_importance <- varImp(fit.knn)

# Print variable importance
print(knn_importance)

#4. Decision trees

# Extract variable importance
dt_importance <- varImp(dt_model)

# Print variable importance
print(dt_importance)


##plot of variable importance
# Extract the variable importance scores
logistic_scores <- logistic_importance$Overall
knn_scores <- c(9.73,85.32,0.00,100.00)
dt_scores <- dt_importance$Overall
rf_scores <- rf_importance

# Create a data frame for plotting
plot_data <- data.frame(
  Model = rep(c("Logistic Regression", "KNN", "Decision Trees", "Random Forest"), each = 4),
  Variable = c("Age", "Daily.Time.Spent.on.Site", "Area.Income", "Daily.Internet.Usage"),
  Importance = c(logistic_scores, knn_scores, dt_scores, rf_scores)
)

# Create the plot
ggplot(plot_data, aes(x = Variable, y = Importance, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Importance", title = "Variable Importance for Different Models") +
  theme_bw() +
  theme(legend.position = "top")




# Create a data frame of variable importance
importance_table <- data.frame(
  Model = rep(c("Logistic Regression", "KNN", "Decision Trees", "Random Forest"), each = 4),
  Variable = c("Age", "Daily.Time.Spent.on.Site", "Area.Income", "Daily.Internet.Usage"),
  Logistic_Importance = logistic_importance$Overall,
  KNN_Importance = c(9.73,85.32,0.00,100.00),
  DT_Importance = dt_importance$Overall,
  RF_Importance = rf_importance
)

# Print the importance table
print(importance_table)
