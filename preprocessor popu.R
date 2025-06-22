library(rvest) 
library(tidyverse) 
library(janitor)

# URL of the website 
url <- "https://www.worldometers.info/world-population/population-by-country/" 

# Read the HTML code of the page 
html_code <- read_html(url) 

# Use the html_nodes function to extract the table 
table_html <- html_code %>% html_nodes("table") %>% .[[1]] 

# Use the html_table function to convert the table  
# HTML code into a data frame 
table_df <- table_html %>% html_table() 
View(table_df)

# Inspect the first few rows of the data frame 
head(table_df)

summary(table_df)

# == DATA PREPROCESSING == 
#first, remove all commas and coerce all applicable columns to numeric
cleaned_df <- table_df %>%
  mutate(across(-c(1, 2), ~ parse_number(as.character(.), 
                                         na = c("", "NA", "N/A"))))

#then turn the percentages into decimals
cleaned_df <- cleaned_df %>%
  mutate(across(c("YearlyChange", "UrbanPop %", "WorldShare"),
                ~ ./100))

#change inappropriate column names
cleaned_df <- cleaned_df %>% janitor::clean_names()

cleaned_df <- cleaned_df %>%
  rename_with(~ str_remove(., "_ordependency"))
cleaned_df <- cleaned_df %>%
  rename_with(~ str_remove(., "_2025"))
#cleaned_df <- cleaned_df %>%
#  rename_with(~ str_remove(., "_p"))
view(cleaned_df)
cleaned_df <- cleaned_df %>% 
  rename(yearly_change_percent = yearly_change)

#finally, save as a new data frame and view
cleaned_df

view(cleaned_df)

write.csv(cleaned_df, "population_data_raw.csv", row.names = FALSE)
