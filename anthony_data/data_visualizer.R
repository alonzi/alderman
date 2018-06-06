# data summary visualizations
# lpa2a
# 2018-06-04
# visualize data

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggthemes)

source('data_cleaner.R')


data_visualizer <- function(tib,filename,volumes){
###########################
# checkouts per day vs checkouts
ggplot(data=tail(tib,34000),aes(x=`Item.Lifetime.Checkout`+`Item.Lifetime.Renewals`,y=checkouts_per_day)) +
  geom_hex()
ggsave(paste(filename,"checkouts_per_dat_vs_total_checkouts+renewals.png"),device='png',dpi=1000)


###########################
# days_since_last_checkout
# shows picture for duration of study - spike at 3045 aka 11/29/2009-12/10/2009
ggplot(data=tib, aes(days_since_last_checkout)) + 
  geom_histogram(bins=500) +
  labs(x = "Days since last checkout", y='Items') +
  theme_bw() + # contender
  scale_x_continuous(limits = c(0.1,6000))
ggsave(paste(filename,"days_since_last checkout.png"),device='png',dpi=1000)

###########################
# shows annual structure of checkout history
ggplot(data=tib, aes(days_since_last_checkout)) + 
  geom_histogram(bins=2000/7) +
  geom_vline(size=.71,xintercept=365, color="red") +
  geom_vline(size=.71,xintercept=2*365, color="blue") +
  geom_vline(size=.71,xintercept=3*365, color="green") +
  geom_vline(size=.71,xintercept=4*365, color="cyan") +
  geom_vline(size=.71,xintercept=5*365, color="magenta") +
  scale_x_continuous(limits = c(0,5.5*365))+
  labs(x = "Days since last checkout", y='Items') +
  theme_bw()  # contender
ggsave(paste(filename,"days_since_last_checkout_structure.png"),device='png',dpi=1000)


# plot S distribution
ggplot(data=tib, aes(S_flat)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))
ggsave(paste(filename,"Sdistribution.png"),device='png',dpi=1000)

# Kept volumes
ggplot(data=tail(tib,volumes), aes(S_flat)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))
ggsave(paste(filename,"Sdistribution_clemons.png"),device='png',dpi=1000)
}
# Usage
#filename <- "B.csv"
#tib <- data_cleaner(filename)
#data_visualizer(tib,filename)
