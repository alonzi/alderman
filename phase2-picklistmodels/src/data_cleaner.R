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

# collapse all dupes into one entry
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumInhouseUses=sum(Item.Lifetime.Inhouse.Uses))
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumCheckouts=sum(Item.Lifetime.Checkout))
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumRenewals=sum(Item.Lifetime.Renewals))

# make count of duplicate ids
data_tibble_uniques <- count(data_tibble,Catalog.Id)

# join dataframes together
data_tibble <- left_join(data_tibble,data_tibble_uniques)

# remove duplicate catalog ids
data_tibble <- distinct(data_tibble,Catalog.Id,.keep_all=TRUE)

# add column for multi volume set
vol1 <-"\\sAbh.*$|\\sAbt.*$|\\san.*$|\\sband*$|\\sbd.*$|\\sBde.*$|\\sbk.*$|\\sbr.*$|\\sBuch*$|\\sc.*$|\\sch.*$|\\scis.*$|\\scuad.*$|\\scz.*$|\\sd.*$|\\sfasc.*$|\\sF.*$|\\sfol.*$|\\sg.*$|\\sHalbbd.*$|\\sHeft*$|\\siss.*$|\\sissue*$|\\sjaarg.*$|\\sJahrg.*$|\\skn.*$|\\skng.*$|\\sl.*$|\\sLfg.*$|\\sn.F.*$|\\snouv.*$|\\sn.r.*$|\\sno.*$|\\snr.*$|\\snu.*$|\\spt.*$|\\squad.*$|\\sReihe*$|\\sr.*$|\\sroc.*$|\\srocz.*$|\\ssb.*$|\\sser.*$|\\sses.*$|\\ssess.*$|\\sSdhft.*$|\\ssuppl.*$|\\ssv.*$|\\sT.*$|\\stbd.*$|\\steil*$|\\stomo*$|\\stome*$|\\sv.*$|\\svyd.*$|\\svyp.*$|\\swyd.*$|\\swydz.*$|\\syr.*$|\\szesz.*$"
vol2 <-"Abh.*$|Abt.*$|an.*$|v.*$|vyd.*$|vyp.*$|wyd.*$|wydz.*$|yr.*$|zesz.*$"
data_tibble <- mutate(data_tibble,MultiVolume=grepl(vol1,Item.Call.Number,ignore.case=TRUE))

# select columns for liaisons
data_tibble <- select(data_tibble,Catalog.Id,Item.Barcode,Item.Call.Number,Item.Library.Code,Catalog.Title,Catalog.Author,Catalog.Pub.Year,Item.Created.Date,Item.Last.Checkout.Date,NumInhouseUses,NumCheckouts,NumRenewals,Duplicates=n,Bib.Marc.Subfield.Data,MultiVolume)

# add column for action
data_tibble <- mutate(data_tibble,KeepOnGrounds=0)
                     
return(data_tibble)
}

### Usage
#filename <- "B.csv"
#Btib <- data_cleaner(filename)


