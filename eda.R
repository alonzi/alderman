######################
# author: lpa2a
# contents: exploratory data analysis
# date: 3/23/18
# github: https://github.com/alonzi/alderman
# purpose: initial exploration of Morton data set

######################
# set up environment
install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)


######################
# read in data
E_tib <- read_excel("E.xlsx")

######################
# compute days since last checkout
# Date Last Checked Out ymd(E_tib$`Date Last Checked Out`)
# Today  use: today()
E_tib <- mutate(E_tib,days_since_last_checkout = today()-ymd(E_tib$`Date Last Checked Out`))

######################
# make the exploratory histograms

# days since last checkout
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram() 
#+  scale_y_log10()

# total checkouts
ggplot(data=tail(E_tib,58888), aes(`Total Checkouts`)) + 
  geom_histogram() +
  scale_y_log10()

# checkouts v days since
# no outliers
ggplot(data=tail(E_tib,58888),aes(x=days_since_last_checkout,y=`Total Checkouts`)) +
  geom_hex()
#with outliers
ggplot(data=E_tib,aes(x=days_since_last_checkout,y=`Total Checkouts`)) +
  geom_hex()
