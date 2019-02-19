##################################################
## Project: Web Scrapings
## Script purpose: Scrape expedia site for flight prices
## Output: csv-file containing flight prices
## Date: 2019-01-10
## Author: Marcus Sjölin
##################################################


# Load relevant packages 

library(readr)      # Read data in data 
library(lubridate)  # Date handling
library(stringr)    # Text handling

library(tidyr)      # Data cleaning
library(dplyr)      # Data cleaning


# Read in city data
city_data <- read.csv("city_data.csv", stringsAsFactors = FALSE)
nbr_cities <- nrow(city_data)

# Set time period for scraping
start_date <- today() + 7
end_date <- start_date + 12
end_date <- start_date + 12
minstay <- 5
maxstay <- 7

maxhop <- "1"
carrier <- "carrier%3ASK"

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

# Do scraping with RSelenium (browsing with automated software) and rvest
library(RSelenium)
library(rvest) # Import data from web

# Loop over all ciy co,binations in list
for (i in 1:nbr_cities) {
  
  # Start server
  driver <- rsDriver()
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
      
      # Loop over all dates in interval, considering minimumstay and maximumstay
      while (departure <= end_date)
      {
        return <- departure + minstay
        
        while (return <= end_date & return - departure <= maxstay)
        {
          print(c(departure, return))

          # Define url
          url <-
            paste0(
              "https://www.expedia.se/Flights-Search?trip=roundtrip&leg1=from%3A",
              origin_link,
              "%2Cto%3A",
              destination_link,
              "%2Cdeparture%3A",
              departure,
              "TANYT&leg2=from%3A",
              destination_link,
              "%2Cto%3A",
              origin_link,
              "%2Cdeparture%3A",
              return,
              "TANYT&passengers=adults%3A1%2Cchildren%3A0%2Cseniors%3A0%2Cinfantinlap%3AY&options=cabinclass%3Aeconomy%2Cmaxhops%3A",
              maxhop,
              "&mode=search&origref=www.expedia.se"
            )
          
          trials <- 0
          scrapeOK<-FALSE
          
          while (!scrapeOK & trials<=3) 
          {

          # Goto url and hold for 5 seconds
          remDr$navigate(url)
          Sys.sleep(15) # Hold 20 seconds
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
              airline = '',
              Origin = origin,
              Destination = destination,
              DepartureDate = departure,
              ReturnDate = return,
              ScrapeDate = today(),
              nbr_trials=trials
            )

          # Add to dataframe and remove the update dataset 
          df <- rbind(df, df_update)
          rm(df_update)

          # Increment return date by one
          return <- return + 1
          
        }
        
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