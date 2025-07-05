
library(e1071)    
library(caret)    
library(dplyr)    


weather_data <- read.csv("hourly_weather_data_final.csv", stringsAsFactors = TRUE)


str(weather_data)
summary(weather_data)


#select relevant columns
selected_cols <- 
  c("temp", "dew", "humidity", "precip", "windspeed", "pressure", 
                   "cloudcover", "visibility", "conditions")


weather_subset <- weather_data[, selected_cols]


sum(is.na(weather_subset))


weather_subset$conditions <- as.factor(weather_subset$conditions)

train_index <- createDataPartition(weather_subset$conditions, p = 0.7, list = FALSE)
train_data <- weather_subset[train_index, ]
test_data <- weather_subset[-train_index, ]

nb_model <- naiveBayes(conditions ~ ., data = train_data)

predictions <- predict(nb_model, test_data)

confusionMatrix(predictions, test_data$conditions)

print(nb_model)
