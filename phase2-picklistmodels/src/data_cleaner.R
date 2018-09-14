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


# establish the canonical variables
data_tibble <- mutate(data_tibble,x0=(365*(2018-data_tibble$Catalog.Pub.Year)-mean(365*(2018-data_tibble$Catalog.Pub.Year)))/sqrt(var(365*(2018-data_tibble$Catalog.Pub.Year))))
data_tibble <- mutate(data_tibble,x1=(today()-mdy(data_tibble$Item.Created.Date)-mean(today()-mdy(data_tibble$Item.Created.Date)))/sqrt(var(today()-mdy(data_tibble$Item.Created.Date))))
data_tibble <- mutate(data_tibble,x2=(data_tibble$Item.Lifetime.Checkout-mean(data_tibble$Item.Lifetime.Checkout))/sqrt(var(data_tibble$Item.Lifetime.Checkout)))
data_tibble <- mutate(data_tibble,x3=(data_tibble$Item.Lifetime.Renewals-mean(data_tibble$Item.Lifetime.Renewals))/sqrt(var(data_tibble$Item.Lifetime.Renewals)))
data_tibble <- mutate(data_tibble,x4=(data_tibble$days_since_last_checkout-mean(data_tibble$days_since_last_checkout))/sqrt(var(data_tibble$days_since_last_checkout)))
data_tibble <- mutate(data_tibble,x5=(data_tibble$checkouts_per_day-mean(data_tibble$checkouts_per_day))/sqrt(var(data_tibble$checkouts_per_day)))



# run the three models
beta <- c(2.*(runif(1)-0.5),2.*(runif(1)-0.5),2.*(runif(1)-0.5),0.,2.*(runif(1)-0.5),0.) # random betas
data_tibble <- mutate(data_tibble,scoreA=1/(1+exp(-(beta[1]*as.numeric(x0)+beta[2]*as.numeric(x1)+beta[3]*as.numeric(x2)+beta[4]*as.numeric(x3)+beta[5]*as.numeric(x4)+beta[6]*as.numeric(x5)))))

data_tibble <- mutate(data_tibble,scoreB=runif(length(x0)))

beta <- c(-1,-1,1,0.,-1,0.) # flat
data_tibble <- mutate(data_tibble,scoreC=1/(1+exp(-(beta[1]*as.numeric(x0)+beta[2]*as.numeric(x1)+beta[3]*as.numeric(x2)+beta[4]*as.numeric(x3)+beta[5]*as.numeric(x4)+beta[6]*as.numeric(x5)))))

data_tibble <- mutate(data_tibble,modelC=dense_rank(1-scoreC))

data_tibble <- mutate(data_tibble,dscoreC=1-scoreC)
data_tibble <- arrange(data_tibble,dscoreC)


cutoffs <- list('./dat/BR_.csv'=7402,
                'DA_.csv'=7024,
                'DS_1-689.csv'=1,
                'DS_701-937.csv'=1,
                'HM_.csv'=5080,
                'HQ_.csv'=9394,
                'PA_.csv'=13103,
                'PG_.csv'=6192,
                'PL_1001-3311.csv'=1,
                'PQ_6001-8560.csv'=1,
                'PR_3991-5990.csv'=1
                )

cutoff <- cutoffs[[filename]]
data_tibble <- mutate(data_tibble,model=ifelse(modelC<cutoff,'1','0'))

# collapse all dupes into one entry
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumInhouseUses=sum(Item.Lifetime.Inhouse.Uses))
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumCheckouts=sum(Item.Lifetime.Checkout))
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumRenewals=sum(Item.Lifetime.Renewals))
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate( MODEL = ifelse( sum(as.integer(model))>=1,'1','0' ) )

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
data_tibble <- select(data_tibble,Catalog.Id,Item.Barcode,Item.Call.Number,Item.Library.Code,Catalog.Title,Catalog.Author,Catalog.Pub.Year,Item.Created.Date,Item.Last.Checkout.Date,NumInhouseUses,NumCheckouts,NumRenewals,Duplicates=n,Bib.Marc.Subfield.Data,MultiVolume,scoreC,MODEL)

# add column for action
data_tibble <- mutate(data_tibble,LiaisonRecommendation=0)
                     


return(data_tibble)
}

### Usage
#filename <- "B.csv"
#Btib <- data_cleaner(filename)


