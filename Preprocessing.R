# Read the CSV file
df <- read.csv('F:/1-2024/data mining/project/archive/Sample - Superstore.csv', encoding = 'latin1')



# Display the first few rows
head(df)

# Display the last few rows
tail(df)

# Display information about the data frame
str(df)

#Summary features 
library(dplyr)
# Identify categorical columns
is_categorical <- function(x) {
  is.character(x) || is.factor(x)
}

# Identify numeric features
numeric_features <- df %>%
  select_if(~is.numeric(.) && !is_categorical(.))

# Identify categorical features
categorical_features <- df %>%
  select_if(is_categorical)

# Identify numeric features
cat("Numeric Features:\n")
head(numeric_features)

# Display the categorical features
cat("\nCategorical Features:\n")
head(categorical_features)


#check if there are missing values 
missing_values <- colSums(is.na(numeric_features))

# Display columns with missing values
cat("Missing Values in Numeric Features:\n")
print(missing_values[missing_values > 0])


#check for duplicate values 
duplicates <- df[duplicated(df), ]

if (nrow(duplicates) > 0) {
  cat("Duplicate Rows:\n")
  print(duplicates)
} else {
  cat("No duplicate rows found.\n")
}


numeric_features 
#delate row ID and postal code 
numeric_features <- subset(numeric_features, select = -c(Row.ID, Postal.Code)) 
numeric_features

#search for outliers in the numerical features 
library(ggplot2)
library(reshape2)

# Create box plots for each numeric feature
boxplot_data <- melt(numeric_features)

# Plot box plots
ggplot(boxplot_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Outliers in Numerical Features",
       x = "Features",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

#see the statistics distribution of the numerical features 
selected_columns <- df[, c('Profit', 'Sales', 'Quantity', 'Discount')]
summary(selected_columns)

df <- subset(df, select = -c(Order.ID, Order.Date, Ship.Date, Customer.ID, Customer.Name, Country, City, State, Product.ID))
df

df <- subset(df, select = -c(Sub.Category, Product.Name))
df


#preprocess data to search correlations: 

# Convert 'Segment' to a factor
df$Segment <- factor(df$Segment, levels = c("Consumer", "Home Office", "Corporate"))

# Convert factor levels to integers
df$Segment <- as.integer(df$Segment)

# Check the unique values in the 'Segment' column
unique(df$Segment)

# Selecting categorical columns for one-hot encoding

categorical_columns <- c('Ship.Mode', 'Region', 'Category')

# Applying one-hot encoding
encoded_features <- model.matrix(~ . - 1, data = df[categorical_columns])

# Generating column names
colnames(encoded_features) <- gsub("([.()\\[\\]\\\\])", "\\\\\\1", colnames(encoded_features))

# Converting into a dataframe
encoded_df <- as.data.frame(encoded_features)

df_reduced <- df[, !(names(df) %in% c(categorical_columns))]

# Concatenating the encoded features 
df <- cbind(df_reduced, encoded_df)
head(df)


colnames(df) <- make.names(colnames(df))
# search the correlation with Random Forest 
library(randomForest)

# split the features and the target variable
X <- df[, !(names(df) %in% c("Segment"))]
y <- df$Segment

print(X)

# Splitting the dataset into training and testing sets
set.seed(42)  
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Initialize the Random Forest Classifier
rf <- randomForest(Segment ~ ., data = df[train_indices, ], ntree = 100, importance = TRUE)

# Getting feature importances
feature_importances <- importance(rf)

MeanDecreaseGini <- feature_importances[, "%IncMSE"]

# Creating a DataFrame to visualize the feature importances
features_df <- data.frame(Feature = rownames(feature_importances), Importance = MeanDecreaseGini)
features_df <- features_df[order(features_df$Importance, decreasing = TRUE), ]

features_df



#Search the correaltion with the target value Profit  

# split the features and the target variable
X <- df[, !(names(df) %in% c("Profit"))]
y <- df$Profit

print(X)

# Splitting the dataset into training and testing sets
set.seed(42)  
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Initialize the Random Forest Classifier
rf <- randomForest(Profit ~ ., data = df[train_indices, ], ntree = 100, importance = TRUE)

# Getting feature importances
feature_importances <- importance(rf)

MeanDecreaseGini <- feature_importances[, "%IncMSE"]

# Creating a DataFrame to visualize the feature importances
features_df <- data.frame(Feature = rownames(feature_importances), Importance = MeanDecreaseGini)
features_df <- features_df[order(features_df$Importance, decreasing = TRUE), ]

features_df




df <- read.csv('F:/1-2024/data mining/project/archive/df.csv', encoding = 'latin1')
head(df)

df <- subset(df, select = -c(RegionSouth, Ship.ModeSame.Day, Ship.ModeFirst.Class, Segment, Ship.ModeStandard.Class))
head(df) 

#Remove outliers: 
remove_outliers <- function(data, cols, threshold = 20) {
  for (col in cols) {
    z_scores <- scale(data[[col]])
    data <- data[abs(z_scores) < threshold, ]
  }
  return(data)
}

# Columns to remove outliers from
columns_to_remove_outliers <- c("Sales", "Profit")

# Remove outliers from selected columns
df <- remove_outliers(df, columns_to_remove_outliers) 
summary(df)

hist(df$Profit, freq = FALSE, main = "Histogram of Profit", xlab = "Profit")

# Overlay normal probability density function
mu <- mean(df$Profit)
sigma <- sd(df$Profit)
x <- seq(min(df$Profit), max(df$Profit), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "red", lwd = 2)

#sales histogram 
hist(df$Sales, freq = FALSE, main = "Histogram of Sales", xlab = "Sales")

# Overlay density function
mu <- mean(df$Sales)
sigma <- sd(df$Sales)
x <- seq(min(df$Sales), max(df$Sales), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "red", lwd = 2)


#define a function to normalize the columns sales and Profit
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
df$Profit <- min_max_normalize(df$Profit)
df$Sales <- min_max_normalize(df$Sales)

head(df)

# see the new distribution after doing normalization
hist(df$Profit, freq = FALSE, main = "Histogram of Profit", xlab = "Profit")
mu <- mean(df$Profit)
sigma <- sd(df$Profit)
x <- seq(min(df$Profit), max(df$Profit), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "red", lwd = 2)


#sales histogram 
hist(df$Sales, freq = FALSE, main = "Histogram of Sales", xlab = "Sales")


mu <- mean(df$Sales)
sigma <- sd(df$Sales)
x <- seq(min(df$Sales), max(df$Sales), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "red", lwd = 2) 

#Download the dataset preprocess
write.csv(df, file = "F:/1-2024/data mining/project/archive/df_process.csv", row.names = FALSE)


summary(df)
