# Read the CSV file
df <- read.csv('F:/1-2024/data mining/project/archive/Sample - Superstore.csv', encoding = 'latin1')
head(df)

#search how often a customre buy
df$Order.Date <- as.Date(df$Order.Date)

# Sort DataFrame 
df <- df[order(df$Customer.ID, df$Order.Date), ]

# Calculate time difference between consecutive orders for each customer
df$Time.Diff <- ave(as.numeric(df$Order.Date), df$Customer.ID, FUN = function(x) c(NA, diff(x)))

# Calculate frequency of orders for each customer
order_frequency$Time.Diff <- as.difftime(order_frequency$Time.Diff, units = "days")

# Display 
print(order_frequency)


#Count the occurences of each customer 
customer_counts <- table(df$Customer.ID)

# Convert the result to a data frame
customer_counts <- as.data.frame(customer_counts)
names(customer_counts) <- c("Customer ID", "Frequency")

# Display the result
print(customer_counts)

summary(customer_counts$Frequency) 


#Search Candidates with the min support >=10 : 
filtered_customers <- subset(customer_counts, Frequency >= 10)

# Display the result
print(filtered_customers)

## Filter the DataFrame to keep only customers with frequency >= 30
top_1_customers <- filtered_customers[filtered_customers$Frequency >= 30, ]

# Display the result
print(top_1_customers)

#prepare the dataset with the column names 

colnames(top_1_customers) <- make.names(colnames(top_1_customers))
print(top_1_customers)

#Search patterns in the clean dataset 
df <- read.csv('F:/1-2024/data mining/project/archive/Sample - Superstore.csv', encoding = 'latin1')

# Filter the original dataset to include only rows related to the filtered customers
filtered_data <- df[df$Customer.ID %in% top_1_customers$Customer.ID, ]

# Select only the specified columns
filtered_data <- filtered_data[, c('Customer.ID', 'Segment', 'Region', 'Category', 'Sub.Category', 'Quantity', 'Discount', 'Profit', 'Sales')]

# Display the result
print(filtered_data)

#Do an analysis in R about the segment of the most frequent customers

segment_counts_f <- table(filtered_data$Segment)

# Display unique classes and their counts
print("Unique classes in the 'Segments' column and their counts:")
print(segment_counts_f)

# Create a pie chart
pie(segment_counts_f, main = "Segment Distribution", col = rainbow(length(segment_counts_f)))

# Add a legend
legend("topright", names(segment_counts_f), fill = rainbow(length(segment_counts_f)))

# Add a title
title(main = "Segment Distribution")


filtered_data
# Select only 'Sales' and 'Profit' columns
sales_profit_data <- filtered_data[, c('Sales', 'Profit')]

# Perform k-means clustering
k <- 3  # Number of clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(sales_profit_data, centers = k)

# Add cluster labels to the original dataframe
filtered_data$Cluster <- kmeans_result$cluster

# Display the cluster centers
print("Cluster Centers:")
print(kmeans_result$centers)

# Display the cluster assignments
print("Cluster Assignments:")
print(table(filtered_data$Cluster))

# Plot the clusters
plot(sales_profit_data, col = kmeans_result$cluster, pch = 20, main = "K-means Clustering: Sales vs. Profit", xlab = "Sales", ylab = "Profit", xlim = c(0, max(sales_profit_data$Sales)), ylim = c(-2000, 2000))
points(kmeans_result$centers, col = 1:k, pch = 4, cex = 2)
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 20)

# Add grid
grid()

# Add labels to the plot
text(sales_profit_data, labels = filtered_data$Cluster, pos = 3)

#Analyze the Cluster 
filtered_data 
unique_clusters <- unique(filtered_data$Cluster)
num_unique_clusters <- length(unique_clusters)

print(num_unique_clusters)
#Segment, region, Category, Sub-Category  

library(arules)


data_subset <- filtered_data[, c("Segment", "Region" ,"Category", "Sub.Category", "Cluster")]
# Convert data to transaction format
transactions <- as(data_subset, "transactions")

# Inspect the transactions
inspect(transactions)

# Run Apriori algorithm
rules <- apriori(transactions, parameter = list(support = 0.2, confidence = 0.8))

# Inspect the rules
inspect(rules)



