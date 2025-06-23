install.packages("caret")
install.packages("corrplot")

# Load libraries
library(dplyr)
library(ggplot2)
library(caret)     # For model training and evaluation
library(corrplot)  # For correlation matrix

# Load and prepare data
data <- read.csv("hourly_weather_data_final.csv")

# Convert datetime
data <- data %>%
  mutate(
    date_parsed = as.Date(datetime),
    hours_conditions = as.factor(hours_conditions)
  )

# Optional: Check for missing values
summary(data)

# Remove rows with missing or extreme outliers in hours_precip
clean_data <- data %>%
  filter(!is.na(hours_precip)) %>%
  filter(hours_precip >= 0)

# Convert categorical conditions to dummy variables
model_data <- clean_data %>%
  select(hours_precip, hours_temp, hours_humidity,
         hours_windspeed, hours_pressure, hours_conditions)

model_data <- model.matrix(hours_precip ~ ., model_data) %>%
  as.data.frame() %>%
  bind_cols(hours_precip = clean_data$hours_precip)

# Split into train/test sets
set.seed(123)
train_index <- createDataPartition(model_data$hours_precip, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set <- model_data[-train_index, ]

# Train linear regression model
model <- lm(hours_precip ~ ., data = train_set)

# Summary of the model
summary(model)

# Predict on test data
predictions <- predict(model, newdata = test_set)

# Evaluate performance
actual <- test_set$hours_precip
rmse <- sqrt(mean((predictions - actual)^2))
mae <- mean(abs(predictions - actual))

cat("RMSE:", round(rmse, 2), "\nMAE:", round(mae, 2), "\n")

# Plot: Actual vs Predicted
ggplot(data.frame(actual, predictions), aes(x = actual, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Precipitation",
       x = "Actual Precipitation (mm)",
       y = "Predicted Precipitation (mm)") +
  theme_minimal()
