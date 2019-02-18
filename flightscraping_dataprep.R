

##################################################
## Project: Web Scrapings - Data visualization
## Script purpose: Visualize data  from web scraping
## Date: 2019-01-10
## Author: Marcus Sjölin
##################################################


library(readr)      # Read data in data 
library(lubridate)  # Date handling
library(stringr)    # Text handling

library(tidyr)      # Data cleaning
library(dplyr)      # Data cleaning

library(ggplot2)    # Visualization


# Set folder and get all csv files 
foldername<-"/home/marcus/R/flight_scraping/scrapedata/"

file.list <- 
  list.files(path=foldername, pattern='*.csv') 

file.list <- unlist(lapply(foldername, paste, file.list, sep=""))

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
  group_by(df, Origin, Destination, DepartureDate, ReturnDate, TravelDates, ScrapeDate) %>%
  top_n(n=-1, wt=price) %>%
  distinct() %>%
  arrange(Journey, DepartureDate, ReturnDate, ScrapeDate) %>%
  select(Journey, Origin, Destination, DepartureDate, ReturnDate, TravelDates, ScrapeDate, price) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor) %>%
  mutate(price_stand = scale(price),
         DaysBeforeDeparture=as.numeric(DepartureDate-ScrapeDate))

# Write prepared dataset 
write.csv(df_lowest, "flightdata.csv", row.names = FALSE)



# Check how many datapoints I have
summary(df_lowest)


table(df_lowest$Origin, df_lowest$Destination)

# 1: check stdeviation per trip

group_by(df_lowest, Journey, Origin, TravelDates) %>%
  summarize(std=sd(price),
            n=n()) %>%
  mutate(std_stand = scale(std)) %>%
  filter(n>=3) %>%
  ggplot(aes(x=Journey, y=std_stand, color=Origin)) +
  geom_point(alpha=0.2) +
  geom_boxplot() +
  geom_hline(yintercept=0) + 
  coord_flip(ylim=c(-1,5)) 

# 2: Check stdeviation per 

# First plot

filter(df_lowest, 
       Origin=="BER" & Destination == "BKK" & DepartureDate=="2019-02-02") %>%
  ggplot(aes(x=ReturnDate, y=price, fill=ScrapeDate, group=ScrapeDate)) + 
  geom_col(position="dodge")
  
