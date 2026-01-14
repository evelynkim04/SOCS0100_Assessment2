## Part I-A Automated Data Collection
# Setup
pacman::p_load(tidyverse, # tidyverse packages including purrr
               purrr, # automating 
               xml2, # parsing XML
               rvest, # parsing HTML
               robotstxt) #checking path is permitted 

# Remove objects
rm(list=ls())

# Setup
pacman::p_load(robotstxt, rvest, dplyr, plotly)

# Path is permitted 
paths_allowed(paths="https://flixpatrol.com/most-watched/")

url <- "https://flixpatrol.com/most-watched/"
parsed <- read_html(url)
parsed.sub <- html_element(parsed, xpath = '/html/body/div[4]/div[1]')

table.df <- html_table(parsed.sub)   
head(table.df)

mostwatched_data <- table.df
head(mostwatched_data)

## Part II-A Building an Interactive Dashboard with R Shiny

