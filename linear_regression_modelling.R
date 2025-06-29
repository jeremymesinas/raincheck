library(dplyr)
library(ggplot2) # Included for potential visualization of predictions later

# 2. Loading the dataset
hourly_weather_data_final <- read.csv("hourly_weather_data_final.csv")

# 3. Data Preprocessing and Feature Selection
hourly_weather_data_final$datetime <- as.Date(hourly_weather_data_final$datetime)

# Select numerical independent variables (features) for 'precip'.
# We avoid variables directly related to 'precip' deviations or 'precipprob'
# as these are derived or highly correlated with the dependent variable.
selected_features <- hourly_weather_data_final %>%
  select(temp, dew, humidity, windgust, windspeed, winddir, pressure,
         cloudcover, visibility, solarradiation, uvindex, severerisk,
         hours_temp, hours_humidity, hours_windgust, hours_windspeed,
         hours_winddir, hours_pressure, hours_visibility, hours_cloudcover,
         hours_solarradiation, hours_severerisk, precip) %>%
  na.omit() # Remove rows with any NAs in selected features to ensure model stability

# 4. Data Splitting (70-30 split for training and testing)
set.seed(123) # for reproducibility

# Determine the number of rows for training data (70%)
train_rows <- sample(1:nrow(selected_features), size = 0.7 * nrow(selected_features))
training_data <- selected_features[train_rows, ]
testing_data <- selected_features[-train_rows, ]

cat("Training data dimensions:", dim(training_data), "\n")
cat("Testing data dimensions:", dim(testing_data), "\n")

# 5. Linear Regression Model Creation
# The dependent variable is 'precip'
# We'll use all other selected features as independent variables using the '.' notation
model_lm <- lm(precip ~ ., data = training_data)

# Summary of the model (provides coefficients, p-values, R-squared for the training set)
cat("\n--- Linear Regression Model Summary (Training Data) ---\n")
print(summary(model_lm))

# 6. Model Prediction on Test Set and Obtaining Prediction Intervals
# Predict 'precip' values on the testing data and calculate prediction intervals.
# 'interval = "prediction"' provides prediction intervals for new observations,
# which account for both the uncertainty in the estimated mean and the random error.
# 'level = 0.95' indicates a 95% prediction interval.
predictions_with_intervals <- predict(model_lm, newdata = testing_data, interval = "prediction", level = 0.95)

# Ensure predictions (and their lower bounds) are non-negative, as precipitation cannot be negative
predictions_with_intervals[predictions_with_intervals < 0] <- 0

# Combine actual values with predictions and their intervals for clarity
results_with_intervals <- data.frame(
  Actual_precip = testing_data$precip,
  Predicted_precip = predictions_with_intervals[, "fit"],
  Lower_Bound_95CI = predictions_with_intervals[, "lwr"],
  Upper_Bound_95CI = predictions_with_intervals[, "upr"]
)

cat("\n--- Sample of Predicted Precipitation with 95% Prediction Intervals (Test Set) ---\n")
# Display the first few rows to show the structure
print(head(results_with_intervals))

# 7. Interpreting Confidence Values (Prediction Intervals)
cat("\n--- Interpreting Confidence Values from Prediction Intervals ---\n")
cat("For each prediction on the test set, the output includes:\n")
cat(" - 'Predicted_precip': The point estimate of precipitation.\n")
cat(" - 'Lower_Bound_95CI': The lower bound of the 95% prediction interval.\n")
cat(" - 'Upper_Bound_95CI': The upper bound of the 95% prediction interval.\n\n")
cat("A 95% prediction interval means that we are 95% confident that the *actual* precipitation\n")
cat("for a new observation (with the given predictor values) will fall within this range.\n")
cat("A narrower interval indicates a more precise (or 'confident') prediction.\n")
cat("Conversely, a wider interval suggests more uncertainty in the individual prediction.\n\n")
cat("These intervals are the 'confidence values' for individual predictions in a regression context.\n")
cat("Unlike classification, where confidence might be a probability score, for regression,\n")
cat("confidence is expressed as a range within which the true value is likely to lie.\n")

# Optional: Visualize a subset of predictions with their intervals
# This helps in understanding the spread of the intervals.
ggplot(head(results_with_intervals, 50), aes(x = 1:50)) +
  geom_point(aes(y = Actual_precip), color = "blue", size = 2, shape = 4) +
  geom_point(aes(y = Predicted_precip), color = "red", size = 2) +
  geom_errorbar(aes(ymin = Lower_Bound_95CI, ymax = Upper_Bound_95CI),
                width = 0.2, color = "gray") +
  labs(title = "Actual vs. Predicted Precipitation with 95% Prediction Intervals (Sample)",
       x = "Observation Index",
       y = "Precipitation Value") +
  scale_color_manual(name = "Value", values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()