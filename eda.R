######################
# author: lpa2a
# contents: exploratory data analysis
# date: 3/23/18
# github: https://github.com/alonzi/alderman
# purpose: initial exploration of data set

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

######################
# make the exploratory histograms

# days since last checkout
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram() 
  #scale_y_log10()

# total checkouts
ggplot(data=tail(E_tib,58888), aes(`Total Checkouts`)) + 
  geom_histogram() +
  scale_y_log10()

# checkouts vs days since
# no outliers checkouts
ggplot(data=tail(E_tib,58888),aes(x=days_since_last_checkout,y=`Total Checkouts`)) +
  geom_hex()
#with outliers
ggplot(data=E_tib,aes(x=days_since_last_checkout,y=`Total Checkouts`)) +
  geom_hex()

### conserved quantity maximum - days + checkouts = C
### actually i don't like this


#####################
# Explanitory histograms

# days since last checkout
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram() 

# annotated and zoomed
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram(bins=2000/7) +
  geom_vline(size=1.2,xintercept=365, color="red") +
  geom_vline(size=1.2,xintercept=2*365, color="blue") +
  geom_vline(size=1.2,xintercept=3*365, color="green") +
  geom_vline(size=1.2,xintercept=4*365, color="cyan") +
  geom_vline(size=1.2,xintercept=5*365, color="magenta") +
  scale_x_continuous(limits = c(0,5.5*365))

### Apply a fourier transform to pick out all the details

# spike at 3000
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram()  +
  scale_x_continuous(limits = c(2000,4000))
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram(bins = 2000/7)  +
  scale_x_continuous(limits = c(2000,4000))
ggplot(data=E_tib, aes(days_since_last_checkout)) + 
  geom_histogram(bins = 50/7)  +
  scale_x_continuous(limits = c(3000,3050))

### ?? what happened 3030 days since data was collected ??

# total checkouts
ggplot(data=tail(E_tib,58888), aes(`Total Checkouts`)) + 
  geom_histogram() +
  scale_y_log10()
ggplot(data=tail(E_tib,58888), aes(`Total Checkouts`)) + 
  geom_histogram(bins=300)
ggplot(data=tail(E_tib,58888), aes(`Total Checkouts`)) + 
  geom_histogram(bins=300) +
  scale_y_log10()

### looks exponential to me

# checkouts vs days since
# no outliers
ggplot(data=tail(E_tib,58888),aes(x=days_since_last_checkout,y=`Total Checkouts`)) +
  geom_hex(bins=2000/14)+
  scale_x_continuous(limits = c(0,2000))
