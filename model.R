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
beta <- c(-1,1) # coefficients days since last checkout,total checkouts
E_tib_nooutliers <- mutate(  E_tib_nooutliers,  S_even = 1/(1+exp(-(beta[1]*as.numeric(days_norm)+beta[2]*czec_norm))))
beta <- c(-5,1) # coefficients days since last checkout,total checkouts
E_tib_nooutliers <- mutate(  E_tib_nooutliers,  S_days = 1/(1+exp(-(beta[1]*as.numeric(days_norm)+beta[2]*czec_norm))))
beta <- c(-1,5) # coefficients days since last checkout,total checkouts
E_tib_nooutliers <- mutate(  E_tib_nooutliers,  S_czec = 1/(1+exp(-(beta[1]*as.numeric(days_norm)+beta[2]*czec_norm))))

# plot S histogram for first test objective
ggplot(data=E_tib_nooutliers, aes(S_even)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))

ggplot(data=E_tib_nooutliers, aes(S_even)) + 
  geom_histogram(bins = 1000) +
  scale_x_continuous(limits = c(0,1))

# checkout that spike
ggplot(data=E_tib_nooutliers,aes(x=S_even,y=days_since_last_checkout)) +
  geom_hex()+
  scale_x_continuous(limits = c(0,1))

# and plot checkouts v S_mixd
ggplot(data=E_tib_nooutliers,aes(x=S_even,y=`Total Checkouts`)) +
  geom_hex()+
  scale_x_continuous(limits = c(0,1))







# plots to show

# even weights
ggplot(data=E_tib_nooutliers, aes(S_even)) + 
  geom_histogram(bins = 250) +
  scale_x_continuous(limits = c(0,1))+
  ggtitle('Model result for even weight')+
  labs(x = "Model Prediction", y='Items') +
  theme_bw()  # contender

# weighted towards days since last checkout
ggplot(data=E_tib_nooutliers, aes(S_days)) + 
  geom_histogram(bins = 250) +
  scale_x_continuous(limits = c(0,1))+
  ggtitle('Model result weight days since checkout')+
  labs(x = "Model Prediction", y='Items') +
  theme_bw()  # contender

# weighted towards total checkouts
ggplot(data=E_tib_nooutliers, aes(S_czec)) + 
  geom_histogram(bins = 250) +
  scale_x_continuous(limits = c(0,1))+
  ggtitle('Model result weight total checkouts')+
  labs(x = "Model Prediction", y='Items') +
  theme_bw()  # contender
