# data prep
# lpa2a
# 2018-06-04
# take data output from director station and clean it for modeling

library(tidyverse)
library(readxl)
library(lubridate)


data_cleaner <- function(filename) {
######################
# read in data
data_tibble <- read.csv(filename)
######################
# compute days since last checkout
# Date Last Checked Out ymd(E_tib$`Date Last Checked Out`)
# Today  use: today()
data_tibble <- mutate(data_tibble,days_since_last_checkout = today()-mdy(data_tibble$Item.Last.Checkout.Date))
# compute total checkouts per days since last checkout
data_tibble <- mutate(data_tibble,checkouts_per_day = as.numeric(data_tibble$Item.Lifetime.Checkout+data_tibble$`Item.Lifetime.Renewals`)/as.numeric(today()-mdy(data_tibble$`Item.Created.Date`)))
return(data_tibble)
}

### Usage
#filename <- "B.csv"
#Btib <- data_cleaner(filename)
