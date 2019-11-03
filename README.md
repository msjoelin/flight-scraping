# Flight Scraping

Project to scrape web for flight prices and analyze the data. 

## Getting Started

1. Download the files 
city_data.csv
If you wish to use other destinations, you have to search out and configure the links in the csv files
2. Set up folder scrapedata (where scrapedata is saved)
3. Run script flightscraping_expedia_oneway.R for scraping data. 
Parameters to be defined:
start_date: first departure date to scrape
end_date: last departure date to be scraped
minstay: minimum number of days between departure and return
maxstay: max number of days between departure and return

### Prerequisites
R and belonging packages, for example

```
library(readr)      
library(lubridate)  
library(stringr)    
library(RSelenium)
library(rvest) 

```

## Built With

* RStudio 


## Authors

* **Marcus Sjölin** 
