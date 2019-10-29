##################################################
## Project: Web Scrapings
## Script purpose: Scrape expedia site for flight prices
## Input: csv-file containing url-text strings
## Output: csv-file containing flight prices
## Date: 2019-10-29
## Author: Marcus Sjölin
##################################################


# Load relevant packages 

library(readr)      # Read data in data 
library(lubridate)  # Date handling
library(stringr)    # Text handling

library(tidyr)      # Data cleaning
library(dplyr)      # Data cleaning

# Scraping RSelenium (browsing with automated software) and rvest
library(RSelenium)
library(devtools)
library(rvest) 


# INPUT 

# Read in city data
city_data <- read.csv("city_data.csv", stringsAsFactors = FALSE)
nbr_cities <- nrow(city_data)

# Set time period for scraping
start_date <- today() + 5
end_date <- start_date + 30

maxhop <- "2"
carrier <- ""  # Leave empty for all airlines
# AA = Aegean, SK = Scandinavian Airlines, LH = Lufthansa, AF = Air France



# Initialize dataframe
df <- data.frame(airline=character(), 
                 price=numeric(),
                 Origin=character(),
                 Destination=character(),
                 DepartureDate=as.Date(character()),
                 ReturnDate=as.Date(character()),
                 ScrapeDate=as.Date(character()),
                 nbr_trials=numeric()
)

# Loop over all ciy combinations in list
for (i in 1:nbr_cities) {
  
  # Start server
  driver <- rsDriver(browser="firefox")
  remDr <- driver[["client"]]
  
    for (j in 1:nbr_cities)  {
    # If origin and destination not is the same, then scrape prices
    if (j != i) {
      
      origin <- city_data$city[i]
      origin_link <-  city_data$linkstring[i]
      destination <- city_data$city[j]
      destination_link <-  city_data$linkstring[j]
      
      print(paste(origin, destination, sep = " "))
      
      # Initialize departure
      departure <- start_date
      
      # Loop over all dates in interval, considering carrier  and maxhop
      while (departure <= end_date)
      {
          print(c(departure))

          # Define url
          url <-
            paste0(
              "https://www.expedia.se/Flights-Search?trip=oneway&leg1=from%3A",
              origin_link,
              "%2Cto%3A",
              destination_link,
              "%2Cdeparture%3A",
              departure,
              "TANYT&passengers=adults%3A1%2Cchildren%3A0%2Cseniors%3A0%2Cinfantinlap%3AY&options=",
              "carrier%3",
              carrier,
              "3%2C",
              "cabinclass%3Aeconomy%2Cmaxhops%3A",
              maxhop,
              "&mode=search&origref=www.expedia.se"
            )
          
          trials <- 0
          scrapeOK<-FALSE
          
          while (!scrapeOK & trials<=3) # Try the scraping 3 times, if not success, move on
          {

          # Goto url and hold for 15 seconds
          remDr$navigate(url)
          Sys.sleep(15) 
          # Get source code and read html
          page <-
            remDr$getPageSource()

          text_xml <- read_html(page[[1]])

          # Get prices
          prices <-
            text_xml %>%
            html_nodes('span.full-bold') %>%
            html_text() %>%
            strsplit("\n") %>%
            unlist(use.names = FALSE) %>%
            as.data.frame()
          
          # Get airline
          airline <-
            text_xml %>%
            html_nodes('div.secondary-content.overflow-ellipsis.inline-children') %>%
            html_text() %>%
            strsplit("\n") %>%
            unlist(use.names = FALSE) %>%
            as.data.frame()
          
          airline$airline <- str_squish(airline$.)
          airline <- select(airline, airline) %>% filter(airline!="")
          
          
          if (nrow(prices)>0) 
            scrapeOK<-TRUE

          trials <- trials+1
          } # End loop
          
          # Check if the scraping was OK, if not, set one NA row as price
          if (nrow(prices)==0) {
            prices <- as.data.frame(NA)
          }
          
          colnames(prices)[1] <- c("price")

          # Create data frame
          df_update <-
            data.frame(prices) %>%
            mutate(
              airline = airline$airline,
              Origin = origin,
              Destination = destination,
              DepartureDate = departure,
              ReturnDate = NA,
              ScrapeDate = today(),
              nbr_trials=trials
            )

          # Add to dataframe and remove the update dataset 
          df <- rbind(df, df_update)
          rm(df_update)

        # Next day
        departure <- departure + 1
      }
      
    }
  }
  # Close session
  remDr$close()
  driver$server$stop()
  
}

# Write data to csv
write.csv(
  df,
  paste0(
    "scrapedata/",
    today(),
    "_",
    hour(now()),
    minute(now()),
    ".csv"
  ),
  row.names = FALSE
)