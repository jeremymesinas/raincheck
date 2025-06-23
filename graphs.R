library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Load the data
data <- read.csv("hourly_weather_data_final.csv")

# Step 1: Parse datetime columns
data <- data %>%
  mutate(
    date_parsed = as.Date(datetime),
    full_datetime = as.POSIXct(paste(datetime, hours_datetime), format = "%Y-%m-%d %H:%M", tz = "Asia/Manila")
  )

# -----------------------------
# GRAPH 1: Today's Line Graph with Flood Warnings
# -----------------------------
today_data <- data %>%
  filter(date_parsed == Sys.Date())

print(paste("Found", nrow(today_data), "rows for today"))

if (nrow(today_data) > 0) {
  ggplot(today_data, aes(x = full_datetime, y = hours_precip)) +
    geom_line(color = "#375a7f", linewidth = 1) +  # Navy blue
    geom_point(aes(color = hours_precip >= 50), size = 3) +
    scale_color_manual(
      values = c("FALSE" = "#7FDBFF", "TRUE" = "#FF4136"),  # Sky blue, Red for warning
      labels = c("Normal", "Flood Warning"),
      name = "Precip Level"
    ) +
    scale_x_datetime(date_labels = "%H:%M", breaks = pretty_breaks(n = 10)) +
    labs(
      x = "Time", y = "Precipitation (mm)",
      title = paste("Hourly Precipitation on", format(Sys.Date(), "%B %d, %Y"))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  warning("No data found for today. Available dates:")
  print(unique(data$date_parsed))
}

# -----------------------------
# GRAPH 2: Stacked Bar - Condition Frequency for Last 7 Days
# -----------------------------
latest_dates <- data %>%
  distinct(date_parsed) %>%
  arrange(desc(date_parsed)) %>%
  slice(1:7) %>%
  pull(date_parsed)

latest_7_days <- data %>%
  filter(date_parsed %in% latest_dates) %>%
  count(date_parsed, hours_conditions) %>%
  mutate(date_parsed = factor(date_parsed, levels = sort(latest_dates)))

ggplot(latest_7_days, aes(x = date_parsed, y = n, fill = hours_conditions)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Weather Condition Frequency (Last 7 Days)",
    x = "Date", y = "Count", fill = "Condition"
  ) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# GRAPH 3: Bar Chart - Peak Precip Per Day
# -----------------------------
peak_precip <- data %>%
  group_by(date_parsed) %>%
  summarise(max_hourly_precip = max(hours_precip, na.rm = TRUE), .groups = 'drop')

ggplot(peak_precip, aes(x = date_parsed, y = max_hourly_precip)) +
  geom_col(fill = "#2E86AB") +  # Deep blue
  geom_text(aes(label = max_hourly_precip), vjust = -0.5, size = 3) +
  labs(
    title = "Peak Hourly Precipitation Per Day",
    x = "Date", y = "Max Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# GRAPH 4: Pie Chart - Overall Weather Condition Frequency
# -----------------------------
condition_freq <- data %>%
  count(hours_conditions) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(condition_freq, aes(x = "", y = n, fill = hours_conditions)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(
    title = "Overall Weather Condition Frequency",
    fill = "Condition"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------
# GRAPH 5: SCATTERPLOT - Hourly Precip for Today (1 point per hour)
# -----------------------------
today_unique <- today_data %>%
  distinct(hours_datetime, .keep_all = TRUE)

ggplot(today_unique, aes(x = hours_datetime, y = hours_precip)) +
  geom_point(color = "#0074D9", size = 3) +  # Vivid blue
  labs(
    title = paste("Hourly Precipitation on", format(Sys.Date(), "%B %d, %Y")),
    x = "Hour", y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# GRAPH 6: LINE GRAPH - Hourly Wind Speed Today
# -----------------------------
if (nrow(today_data) > 0) {
  ggplot(today_data, aes(x = full_datetime, y = hours_windspeed)) +
    geom_line(color = "#20B2AA", linewidth = 1) +  # LightSeaGreen / Teal
    geom_point(color = "#00CED1", size = 2) +      # DarkTurquoise
    scale_x_datetime(
      date_labels = "%H:%M",
      breaks = pretty_breaks(n = 10)
    ) +
    labs(
      title = paste("Hourly Wind Speeds on", format(Sys.Date(), "%B %d, %Y")),
      x = "Time",
      y = "Wind Speed (km/h)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
