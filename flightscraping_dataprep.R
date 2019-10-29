

##################################################
## Project: Web Scrapings - Data visualization
## Script purpose: Consolidate and prepare data from web scraping csv-files
## Input: CSV-files in scrapedata-folder
## Output: 
## Date: 2019-01-10
## Author: Marcus Sjölin
##################################################


library(readr)      # Read data in data 
library(lubridate)  # Date handling
library(stringr)    # Text handling

library(tidyr)      # Data cleaning
library(dplyr)      # Data cleaning

library(ggplot2)    # Visualization

# Get all csv files from scrapedata folder
file.list <- 
  list.files(path="scrapedata/", pattern='*.csv') 

file.list <- unlist(lapply("scrapedata/", paste, file.list, sep=""))

# Read in all files and combine to df
df.list <- lapply(file.list, read_csv)

df <- bind_rows(df.list)

# rm(df.list, file.list, foldername)

head(df)

# Extract prices
prices <-  
  str_extract_all(df$price, "\\-*\\d+\\.*\\d*") %>%
  lapply(paste, collapse="") %>%
  unlist()

df <- mutate(df, 
             price=as.numeric(prices), 
             Journey=paste(Origin, Destination, sep="-"),
             TravelDates=paste(DepartureDate, ReturnDate, sep="-"))

#### clean dataset so only same kind datapoints are used 

# Only get lowest prices for each group
df_lowest <- 
  group_by(df, airline, Origin, Destination, DepartureDate, ReturnDate, TravelDates, ScrapeDate) %>%
  top_n(n=-1, wt=price) %>%
  distinct() %>%
  arrange(Journey, airline, DepartureDate, ReturnDate, ScrapeDate) %>%
  ungroup() %>%
  mutate(DaysBeforeDeparture=as.numeric(DepartureDate-ScrapeDate)) 

# Write prepared dataset 
write.csv(df_lowest, "flightdata.csv", row.names = FALSE)

# Update file in google drive
library(googledrive)

# Update matches
x<- drive_update(file=as_id("12lzKj1t0g1maoO6FwX8c8lmehD95mt-7fb7zJ3VrFiU"),
                 media="flightdata.csv")

