# Setting up
pacman::p_load(tidyverse, # tidyverse pkgs including purrr
               purrr, # automating 
               xml2, # parsing XML
               rvest, # parsing HTML
               robotstxt) #checking path is permitted 

# Parsing URL of website
library(rvest)
library(xml2)
url <- "https://www.netflix.com/tudum/top10"
parsed <- read_html(url)

# Selecting the desired part
parsed.sub <- html_element(parsed, xpath = '//*[@id="appMountPoint"]/div/div/div[1]/div/div[2]/section[4]/div/div[2]/table')

# Converting to table
table.df <- html_table(parsed.sub)   
head(table.df)

# Checking whether path is allowed to be scraped 
paths_allowed(paths="https://www.netflix.com/tudum/top10")

# Creating URL list for the websites to be scraped 
url_list <- c("https://www.netflix.com/tudum/top10")