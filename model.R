######################
# author: lpa2a
# contents: model to predict book use
# date: 3/23/18
# github: https://github.com/alonzi/alderman
# purpose: initial model

######################
# set up environment
#install.packages("tidyverse")

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

# remove high checkout outliers
E_tib_nooutliers <- tail(E_tib,58888)

# make scaled columns
E_tib_nooutliers <- mutate(E_tib_nooutliers,days_norm=days_since_last_checkout/max(as.numeric(E_tib_nooutliers$days_since_last_checkout),na.rm=TRUE))
E_tib_nooutliers <- mutate(E_tib_nooutliers,czec_norm=E_tib_nooutliers$`Total Checkouts`/max(E_tib_nooutliers$`Total Checkouts`,na.rm = TRUE))

# make S
E_tib_nooutliers <- mutate(  E_tib_nooutliers,  S_mixd = 1/(1+exp(-(-as.numeric(days_norm)+czec_norm))))

# plot S histogram for first test objective
ggplot(data=E_tib_nooutliers, aes(S_mixd)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))

ggplot(data=E_tib_nooutliers, aes(S_mixd)) + 
  geom_histogram(bins = 1000) +
  scale_x_continuous(limits = c(0,1))

# checkout that spike
ggplot(data=E_tib_nooutliers,aes(x=S_mixd,y=days_since_last_checkout)) +
  geom_hex()+
  scale_x_continuous(limits = c(0,1))

# and plot checkouts v S_mixd
ggplot(data=E_tib_nooutliers,aes(x=S_mixd,y=`Total Checkouts`)) +
  geom_hex()+
  scale_x_continuous(limits = c(0,1))

