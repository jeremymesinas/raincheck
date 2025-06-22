library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Load the data
data <- read.csv("hourly_weather_data_final.csv")

# Convert datetime from "YYYY-MM-DD" to Date type
data <- data %>%
  mutate(
    date_parsed = as.Date(datetime),  # This automatically handles "YYYY-MM-DD" format
    full_datetime = as.POSIXct(paste(datetime, hours_datetime))  # Combines date and time
  )

# Filter for today's date (using proper date comparison)
today_data <- data %>%
  filter(date_parsed == Sys.Date())

# Debug: Check what we found
print(paste("Found", nrow(today_data), "rows for today"))
head(today_data)

# Precipitation Line Graph
if (nrow(today_data) > 0) {
  ggplot(today_data, aes(x = full_datetime, y = hours_precip)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(aes(color = hours_precip >= 50), size = 3) +
    scale_color_manual(
      values = c("FALSE" = "green", "TRUE" = "red"),
      labels = c("Normal", "Flood Warning"),
      name = "Precip Level"
    ) +
    scale_x_datetime(
      date_labels = "%H:%M",
      breaks = pretty_breaks(n = 10)
    ) +
    labs(
      x = "Time",
      y = "Precipitation (mm)",
      title = paste("Hourly Precipitation on", format(Sys.Date(), "%B %d, %Y"))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
} else {
  warning("No data found for today. Available dates:")
  print(unique(data$date_parsed))
}

# Get the latest 7 unique dates from the dataset (in case some days have missing data)
latest_dates <- data %>%
  distinct(date_parsed) %>%
  arrange(desc(date_parsed)) %>%
  slice(1:7) %>%
  pull(date_parsed)

# Filter the original data to include only these 7 dates
latest_7_days <- data %>%
  filter(date_parsed %in% latest_dates) %>%
  count(date_parsed, hours_conditions)

# Ensure the bars are in chronological order
latest_7_days <- latest_7_days %>%
  mutate(date_parsed = factor(date_parsed, levels = sort(latest_dates)))

# Plot
ggplot(latest_7_days, aes(x = date_parsed, y = n, fill = hours_conditions)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Weather Condition Frequency (Last 7 Days)",
    x = "Date",
    y = "Count",
    fill = "Condition"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )