# 1. Install necessary packages if you haven't already
# install.packages(c("readr", "cluster", "factoextra", "dplyr"))

# Load the installed packages
library(readr)
library(cluster)
library(factoextra)
library(dplyr)

# 2. Load your CSV file
weather_data <- read_csv("hourly_weather_data_final.csv")

# 3. Select relevant numerical 'hours_' columns for clustering
clustering_data <- weather_data %>%
  select(
    starts_with("hours_"),
    -hours_datetime,
    -hours_conditions,
    -ends_with("_outlier"),
    -ends_with("_deviation")
  )

# 4. Convert all columns to numeric and handle infinite values
clustering_data <- clustering_data %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))

# 5. Handle missing values
clustering_data_clean <- na.omit(clustering_data)

# Check dimensions
cat("Dimensions before cleaning:", dim(clustering_data), "\n")
cat("Dimensions after cleaning:", dim(clustering_data_clean), "\n")

# 6. Scale the data
scaled_data <- scale(clustering_data_clean)

# 7. Determine optimal number of clusters (Elbow Method)
set.seed(123)
elbow_plot <- fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")
print(elbow_plot)

# 8. Apply K-Means clustering (using k=3 as example)
k_optimal <- 3
kmeans_result <- kmeans(scaled_data, centers = k_optimal, nstart = 25)
print(kmeans_result)

# 9. Add cluster assignments
clustering_data_clean$cluster <- kmeans_result$cluster

# 10. Visualize clusters
cluster_plot <- fviz_cluster(kmeans_result, data = scaled_data,
                             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
                             geom = "point",
                             ggtheme = theme_minimal()) +
  labs(title = paste("K-Means Clustering (k =", k_optimal, ")"))
print(cluster_plot)

# 11. Analyze cluster characteristics
cluster_means <- clustering_data_clean %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

print(cluster_means)

# 12. Silhouette method for optimal k
silhouette_plot <- fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
print(silhouette_plot)
