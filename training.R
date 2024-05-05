#Regresion model to predict Profit 

df <- read.csv('F:/1-2024/data mining/project/archive/df_process.csv', encoding = 'latin1')
head(df)



#Preparing dataset 
library(caret)



# Set the seed 
set.seed(123)

# Split the dataset into 80% training and 20% testing
train_index <- createDataPartition(df$Profit, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Separate features and target variable in training and testing data
X_train <- train_data[, !colnames(train_data) %in% c("Profit")]
y_train <- train_data$Profit
X_test <- test_data[, !colnames(test_data) %in% c("Profit")]
y_test <- test_data$Profit

start_time <- Sys.time()


# Load necessary libraries
library(glmnet)


# Function to calculate evaluation metrics
calculate_metrics <- function(y_true, y_pred) {
  # R-squared (R²)
  rsquared <- cor(y_true, y_pred)^2
  
  # Mean Squared Error (MSE)
  mse <- mean((y_true - y_pred)^2)
  
  # Mean Absolute Percentage Error (MAPE)
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  
  # Mean Absolute Error (MAE)
  mae <- mean(abs(y_true - y_pred))
  
  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  
  # Return metrics
  return(list(RSquared = rsquared, MSE = mse, MAPE = mape, MAE = mae, RMSE = rmse))
}

# Function to plot y_test vs. y_pred
plot_predictions <- function(y_test, y_pred, title) {
  # Create a data frame with y_test and y_pred
  plot_data <- data.frame(y_test = y_test, y_pred = y_pred)
  
  # Create scatter plot
  plot <- ggplot(plot_data, aes(x = y_test, y = y_pred)) +
    geom_point(color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = title, x = "Actual", y = "Predicted")
  
  # Return plot
  return(plot)
}

# Linear regression
lm_model <- lm(y_train ~ ., data = X_train)
lm_predictions <- predict(lm_model, newdata = X_test)
lm_metrics <- calculate_metrics(y_test, lm_predictions)
lm_plot <- plot_predictions(y_test, lm_predictions, "Linear Regression")

# Lasso regression
lasso_model <- train(
  x = as.matrix(X_train),
  y = y_train,
  method = "glmnet",
  trControl = trainControl("cv"),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, by = 0.01))
)
lasso_predictions <- predict(lasso_model, newdata = as.matrix(X_test))
lasso_metrics <- calculate_metrics(y_test, lasso_predictions)
lasso_plot <- plot_predictions(y_test, lasso_predictions, "Lasso Regression")

# Ridge regression
ridge_model <- train(
  x = as.matrix(X_train),
  y = y_train,
  method = "glmnet",
  trControl = trainControl("cv"),
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, by = 0.01))
)
ridge_predictions <- predict(ridge_model, newdata = as.matrix(X_test))
ridge_metrics <- calculate_metrics(y_test, ridge_predictions)
ridge_plot <- plot_predictions(y_test, ridge_predictions, "Ridge Regression")

# ElasticNet regression
elasticnet_model <- train(
  x = as.matrix(X_train),
  y = y_train,
  method = "glmnet",
  trControl = trainControl("cv"),
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0.001, 1, by = 0.01))
)
elasticnet_predictions <- predict(elasticnet_model, newdata = as.matrix(X_test))
elasticnet_metrics <- calculate_metrics(y_test, elasticnet_predictions)
elasticnet_plot <- plot_predictions(y_test, elasticnet_predictions, "ElasticNet Regression")

# Print metrics
print("Linear Regression Metrics:")
print(lm_metrics)
print("Lasso Regression Metrics:")
print(lasso_metrics)
print("Ridge Regression Metrics:")
print(ridge_metrics)
print("ElasticNet Regression Metrics:")
print(elasticnet_metrics)

# Plot predictions
print(lm_plot)
print(lasso_plot)
print(ridge_plot)
print(elasticnet_plot)


# End measuring time
end_time <- Sys.time()

# Compute the duration
duration <- end_time - start_time

# Print the duration
print(duration)
 
df <- read.csv('F:/1-2024/data mining/project/archive/df_process.csv', encoding = 'latin1')
head(df)




train_index <- sample(1:nrow(df), nrow(df)*0.7)

# train dataset formation
train_set <- df[train_index, ]
str(train_set)

# test dataset formation
test_set <- df[-train_index, ]
str(test_set)


library(rpart)

# Build the decision tree model
profit_tree <- rpart(Profit ~ ., data = df, method = "anova")

# Print the summary of the tree
summary(profit_tree)

library(rpart.plot)
rpart.plot(profit_tree, main = "Decision Tree for the Profit Target") 



predictions <- predict(profit_tree, test_set)
predictions

mae <- mean(abs(predictions - test_set$Profit))
mse <- mean((predictions - test_set$Profit)^2)
rmse <- sqrt(mse)

# Print the evaluation metrics
print(paste("Mean Absolute Error:", mae))
print(paste("Mean Squared Error:", mse))
print(paste("Root Mean Squared Error:", rmse)) 


# Create a scatter plot of actual vs predicted values
plot(test_set$Profit, predictions, 
     xlab = "Actual Profit", ylab = "Predicted Profit",
     main = "Actual vs Predicted Profit",
     col = "blue", pch = 16)

# Add a diagonal line representing perfect predictions
abline(0, 1, col = "red")

# Add a legend
legend("topleft", legend = c("Actual vs Predicted", "Perfect Prediction"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1)) 

#change the profit since numerical to categorical

df$Profit_Category <- cut(df$Profit, breaks = c(0, 1/3, 2/3, 1), labels = c(1, 2, 3))

# Display the updated dataframe
print(df)

# Delete the "Profit" column
df <- df[, !colnames(df) %in% c("Profit")]

# Display the updated dataframe
print(df) 

train_index <- sample(1:nrow(df), nrow(df)*0.7)

# train dataset 
train_set <- df[train_index, ]
str(train_set)

# test dataset 
test_set <- df[-train_index, ]
str(test_set)


# Build the decision tree model
profit_tree <- rpart(Profit_Category ~ ., data = df, method = "class")

# Print the summary of the tree
summary(profit_tree)

library(rpart.plot)
rpart.plot(profit_tree, main = "Decision Tree for the Profit Target") 



predictions <- predict(profit_tree, test_set, type = "class")
predictions


cm <- confusionMatrix(predictions, test_set$Profit_Category)
cm

#save the model and download 
saveRDS(profit_tree, file = "profit_tree.rds")
destination_directory <- "F:/1-2024/data mining/project/archive/"

file.copy("profit_tree.rds", paste0(destination_directory, "profit_tree.rds"))


