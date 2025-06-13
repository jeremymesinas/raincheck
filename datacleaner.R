#read csv
data <- read.csv("weather_hourly_data.csv")

#summary checking functions
head(data)
tail(data)
dim(data)
str(data)
summary(data)

#remove irrelevant and excessive unique identifiers
cols_to_remove <- c(
  "datetimeEpoch", "tempmax", "tempmin", "feelslikemax", "feelslikemin", "feelslike",
  "solarenergy", "sunrise", "sunriseEpoch", "sunset", "sunsetEpoch",
  "moonphase", "source", "hours_datetimeEpoch", "hours_feelslike",
  "hours_dew", "hours_solarenergy", "hours_uvindex", "hours_icon",
  "hours_source", "date", "snow", "snowdepth", "hours_snow", "hours_snowdepth"
)

data <- data[ , !(names(data) %in% cols_to_remove)]

str(data)

#null values checker, which are none
total_na <- sum(is.na(data))
print(total_na)

#duplicate values checker, which are none
duplicate_rows_logical <- duplicated(data)
print(duplicate_rows_logical)

num_duplicates <- sum(duplicate_rows_logical)
print(paste("Number of duplicate rows (exact duplicates):", num_duplicates))

#check the outliers for every numerical column
output_dir <- "with outliers"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Get only numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Create boxplots for original data
for (col_name in names(numeric_data)) {
  png_filename <- file.path(output_dir, paste0(col_name, ".png"))
  
  png(png_filename, width = 1200, height = 800)
  boxplot(numeric_data[[col_name]],
          main = paste("Boxplot of", col_name),
          ylab = col_name)
  dev.off()
}

# Flag outliers instead of deleting them
# Because outliers can indicate flood risks
numeric_cols <- sapply(data, is.numeric)

# Create outlier flags for each numeric column
for (col_name in names(data)[numeric_cols]) {
  IQR_value <- IQR(data[[col_name]], na.rm = TRUE)
  lower_bound <- quantile(data[[col_name]], 0.25, na.rm = TRUE) - 1.5 * IQR_value
  upper_bound <- quantile(data[[col_name]], 0.75, na.rm = TRUE) + 1.5 * IQR_value
  
  # Create new column with _outlier suffix
  new_col_name <- paste0(col_name, "_outlier")
  data[[new_col_name]] <- ifelse(
    data[[col_name]] < lower_bound | data[[col_name]] > upper_bound,
    TRUE,  # Mark as outlier
    FALSE  # Not an outlier
  )
  
  #Add magnitude of deviation
  deviation_col <- paste0(col_name, "_deviation")
  data[[deviation_col]] <- ifelse(
    data[[new_col_name]],
    ifelse(
      data[[col_name]] < lower_bound,
      (lower_bound - data[[col_name]]) / IQR_value,  # How many IQRs below
      (data[[col_name]] - upper_bound) / IQR_value   # How many IQRs above
    ),
    0  # No deviation for non-outliers
  )
}

# Show summary of outlier counts
outlier_cols <- grep("_outlier$", names(data), value = TRUE)
print("Outlier counts per variable:")
print(sapply(data[outlier_cols], sum))


#Save boxplots with flagged outliers
output_dir <- "with outlier flags"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Create boxplots showing outliers
for (col_name in names(numeric_data)) {
  png_filename <- file.path(output_dir, paste0(col_name, ".png"))
  
  png(png_filename, width = 1200, height = 800)
  boxplot(numeric_data[[col_name]],
          main = paste("Boxplot of", col_name, "(outliers flagged)"),
          ylab = col_name,
          outline = FALSE) 
  
  # Add points for flagged outliers
  outliers <- data[[paste0(col_name, "_outlier")]]
  points(which(outliers), numeric_data[[col_name]][outliers], 
         col = "red", pch = 19)
  dev.off()
}

#decimal score normalization for all numeric columns
decimal_scaling <- function(x) {
  if (is.numeric(x)) {
    max_val <- max(abs(x), na.rm = TRUE)
    if (max_val == 0) return(x) 
    scale_factor <- 10 ^ ceiling(log10(max_val))
    return(x / scale_factor)
  } else {
    return(x)  
  }
}

data <- as.data.frame(lapply(data, decimal_scaling))

#write CSV with outlier flags included
write.csv(data, "hourly_weather_data_final.csv", row.names = FALSE)
