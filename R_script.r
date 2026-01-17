## Part I-A Automated Data Collection
# Setup
pacman::p_load(tidyverse, # tidyverse packages including purrr
               purrr, # automating 
               xml2, # parsing XML
               rvest, # parsing HTML
               robotstxt) #checking path is permitted 

# Removing objects in environment
rm(list=ls())

# Setup
pacman::p_load(robotstxt, rvest, dplyr, plotly)

# Checking if path is permitted 
paths_allowed(paths="https://flixpatrol.com/most-watched/2025-1/titles-from-united-kingdom/")

url <- "https://flixpatrol.com/most-watched/2025-1/titles-from-united-kingdom/"
parsed <- read_html(url) # This reads the HTML content of the page. 
parsed.sub <- html_element(parsed, xpath = '/html/body/div[4]/div[1]') # This extracts the specific xpath of the table containing the wanted most-watched data. 

# Converting the data into a table
table.df <- html_table(parsed.sub) # This converts the extracted HTML table into an R dataframe.   
head(table.df)

## Part I-B Data Exploration and Contextualisation

# Rename the first column to "rank"
names(table.df)[1] <- "rank"

# Keeping all columns except "country" since it is all the UK
mostwatched_data <- table.df %>% select("rank", "title", "type", "premiere", "genre", "hours", "runtime", "views") # Dropping the "country" column since all data is for the UK.
head(mostwatched_data)

# Tidying data
library(janitor) # Loading the Janitor package.

# Cleaning names
names(mostwatched_data) <-  janitor::make_clean_names(names(mostwatched_data)) # This cleans all names into snake_case. 

# Deleting empty rows
empt <- apply(mostwatched_data, 1, FUN = function(x) all(is.na(x) | x == "")) # Removing missing values or empty rows. 
mostwatched_data <- mostwatched_data[which(!empt), ] 

head(mostwatched_data)

# Deleting the first row
mostwatched_data <- mostwatched_data[-1, ] # Removing the first row since it's empty. 

# Deleting the repeated second value in columns "hours" and "views"
mostwatched_data$views <- sub("\\s.*", "", mostwatched_data$views) # Each cell in "hours" and "views" had the same information written twice. This line thus keeps only one number. 
mostwatched_data$hours <- sub("\\s.*", "", mostwatched_data$hours)

# Final table
library(kableExtra)
head(mostwatched_data, 5) |>
  kbl(caption = "Mostwatched TV/Movies UK", booktabs = TRUE) |>
  kable_styling(full_width = FALSE, position = "center")

# Creating "mostwatched_data" CSV for shiny app
write.csv(mostwatched_data, "mostwatched_data.csv", row.names = FALSE) # Exporting the cleaned "mostwatched_data" dataset as CSV for use in Shiny app. 

