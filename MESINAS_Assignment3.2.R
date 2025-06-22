population_data_raw <- read.csv("population_data_raw.csv")

#task 1, identify countries with high population
high_pop_threshold <- 100000000

high_pop_countries <- c()

for (i in 1:nrow(population_data_raw)) {
  current_population <- population_data_raw$population[i]
  
  if (current_population >= high_pop_threshold) {
    # if <= threshold, add to vector
    high_pop_countries <- c(high_pop_countries, population_data_raw$country[i])
  }
}

if (length(high_pop_countries) > 0) {
  print("Countries with high population:")
  print(high_pop_countries)
} else {
  print("No countries available.\n")
}

#task 2: Categorizing Countries by Yearly Change
population_data_raw$Change_Category <- NA

for (i in 1:nrow(population_data_raw)) {
  yearly_change <- population_data_raw$yearly_change_percent[i]
  
  if (yearly_change > 0.015) {
    population_data_raw$Change_Category[i] <- "High Growth"
  } else if (yearly_change > 0.005) {
    population_data_raw$Change_Category[i] <- "Moderate Growth"
  } else if (yearly_change >= 0) {
    population_data_raw$Change_Category[i] <- "Low Growth/Stable"
  } else {
    population_data_raw$Change_Category[i] <- "Decline"
  }
}

head(population_data_raw[c("country", "yearly_change_percent", "Change_Category")], 10)

#task 3: Analyzing Population Density
get_density_description <- function(density) {
  if (density < 25) {
    return("Very Sparse")
  } else if (density >= 25 & density < 100) {
    return("Sparse")
  } else if (density >= 100 & density < 500) {
    return("Moderate")
  } else if (density >= 500 & density < 2000) {
    return("Dense")
  } else if (density >= 2000) {
    return("Very Dense")
  } else {
    return(NA)  
  }
}

# Apply the function and check
population_data_raw$Density_Category <- sapply(population_data_raw$density, get_density_description)
head(population_data_raw[c("country", "density_p_km2", "Density_Category")], 10)

#Task 4: Visualizing Data
library(ggplot2)

#4.1 bar chart for populout countries
high_pop <- subset(population_data_raw, population >= high_pop_threshold)

ggplot(high_pop, aes(x = reorder(country, -population), y = population)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "High Population Countries",
       x = "Country",
       y = "Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

#4.2 bar chart for country count
ggplot(population_data_raw, aes(x = Change_Category)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Countries by Population Growth Category",
       x = "Growth Category",
       y = "Number of Countries") +
  theme_minimal() +
  scale_x_discrete(limits = c("High Growth", "Moderate Growth", "Low Growth/Stable", "Decline"))

#4.3 bar chart for density category
ggplot(population_data_raw, aes(x = Density_Category)) +
  geom_bar(fill = "darkblue") +
  labs(title = "Distribution of Countries by Population Density",
       x = "Density Category",
       y = "Number of Countries") +
  theme_minimal() +
  scale_x_discrete(limits = c("Very Sparse", "Sparse", "Moderate", "Dense", "Very Dense")) 