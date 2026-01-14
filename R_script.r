## Part I-A Automated Data Collection
# Setup
pacman::p_load(tidyverse, # tidyverse packages including purrr
               purrr, # automating 
               xml2, # parsing XML
               rvest, # parsing HTML
               robotstxt) #checking path is permitted 

# Removing objects
rm(list=ls())

# Setup
pacman::p_load(robotstxt, rvest, dplyr, plotly)

# Checking if path is permitted 
paths_allowed(paths="https://flixpatrol.com/most-watched/2025-1/titles-from-united-kingdom/")

url <- "https://flixpatrol.com/most-watched/2025-1/titles-from-united-kingdom/"
parsed <- read_html(url)
parsed.sub <- html_element(parsed, xpath = '/html/body/div[4]/div[1]')

# Converting the data into a table
table.df <- html_table(parsed.sub)   
head(table.df)

## Part I-B Data Exploration and Contextualisation

# Rename the first column to "rank"
names(table.df)[1] <- "rank"

# Keeping all columns except "country" since it is all the UK
mostwatched_data <- table.df %>% select("rank", "title", "type", "premiere", "genre", "hours", "runtime", "views")
head(mostwatched_data)

# Tidying data
library(janitor)

# Cleaning names
names(mostwatched_data) <-  janitor::make_clean_names(names(mostwatched_data))

# Deleting empty rows
empt <- apply(mostwatched_data, 1, FUN = function(x) all(is.na(x) | x == ""))
mostwatched_data <- mostwatched_data[which(!empt), ]

head(mostwatched_data)

# Deleting the first row
mostwatched_data <- mostwatched_data[-1, ]
head(mostwatched_data)

