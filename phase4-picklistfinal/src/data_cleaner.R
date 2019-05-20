# data prep
# lpa2a
# 2018-06-04
# take data output from director station and clean it for modeling

### Usage
#filename <- "B.csv"
#Btib <- data_cleaner(filename)


library(tidyverse)
library(readxl)
library(lubridate)


data_cleaner <- function(filename) {
######################
# read in data
#data_tibble <- read.csv(filename,fileEncoding="latin1") # use for PC_.csv
data_tibble <- read.csv(filename,encoding="UTF-8")
print('analyzing')
print(filename)
######################
# compute days since last checkout
# Date Last Checked Out ymd(E_tib$`Date Last Checked Out`)
# Today  use: today()
data_tibble <- mutate(data_tibble,days_since_last_checkout = today()-mdy(data_tibble$Item.Last.Checkout.Date))
# compute total checkouts per days since last checkout
data_tibble <- mutate(data_tibble,Item.Lifetime.Checkout.Corrected=Item.Legacy.Total.Charges-Item.Lifetime.Renewals)

data_tibble <- mutate(data_tibble,checkouts_per_day = as.numeric(data_tibble$Item.Lifetime.Checkout.Corrected+data_tibble$`Item.Lifetime.Renewals`)/as.numeric(today()-mdy(data_tibble$`Item.Created.Date`)))

# establish the canonical variables
data_tibble <- mutate(data_tibble,x0=(365*(2018-data_tibble$Catalog.Pub.Year)-mean(365*(2018-data_tibble$Catalog.Pub.Year)))/sqrt(var(365*(2018-data_tibble$Catalog.Pub.Year))))
data_tibble <- mutate(data_tibble,x1=(today()-mdy(data_tibble$Item.Created.Date)-mean(today()-mdy(data_tibble$Item.Created.Date)))/sqrt(var(today()-mdy(data_tibble$Item.Created.Date))))

data_tibble <- mutate(data_tibble,x2=(data_tibble$Item.Lifetime.Checkout.Corrected-mean(data_tibble$Item.Lifetime.Checkout.Corrected))/sqrt(var(data_tibble$Item.Lifetime.Checkout.Corrected)))

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
# put back
#data_tibble <- arrange(data_tibble,dscoreC)


cutoffs <- list('./data/AM_AS_AZ_.csv'=811,
'./data/BC_.csv'=531,
'./data/BD_.csv'=1939,
'./data/BF_.csv'=4219,
'./data/BH_.csv'=677,
'./data/BJ_.csv'=2163,
'./data/BL_.csv'=6550,
'./data/BM_.csv'=1987,
'./data/BP_.csv'=3497,
'./data/BQ_.csv'=3301,
'./data/BR_.csv'=6158,
'./data/BS_.csv'=5064,
'./data/BT_.csv'=3564,
'./data/BV_.csv'=2023,
'./data/BX_.csv'=6408,
'./data/B_.csv'=12595,
'./data/C_CT_.csv'=2864,
'./data/DAW_.csv'=16,
'./data/DA_.csv'=5369,
'./data/DB_.csv'=269,
'./data/DC_.csv'=2826,
'./data/DD_.csv'=1692,
'./data/DE_.csv'=294,
'./data/DF_.csv'=1476,
'./data/DG_.csv'=2390,
'./data/DH_.csv'=84,
'./data/DJK_.csv'=121,
'./data/DJ_.csv'=56,
'./data/DK_.csv'=2486,
'./data/DL_.csv'=158,
'./data/DP_.csv'=976,
'./data/DQ_.csv'=30,
'./data/DR_.csv'=758,
'./data/DS_.csv'=15005,
'./data/DT_.csv'=4315,
'./data/DU_.csv'=505,
'./data/DX_.csv'=25,
'./data/D_.csv'=7067,
'./data/E_.csv'=26303,
'./data/F_.csv'=12308,
'./data/G__GV_.csv'=9741,
'./data/HA_.csv'=238,
'./data/HB_.csv'=3343,
'./data/HC_.csv'=5016,
'./data/HD_.csv'=6387,
'./data/HE_.csv'=667,
'./data/HF_.csv'=2567,
'./data/HG_.csv'=2413,
'./data/HJ_.csv'=445,
'./data/HM_.csv'=4452,
'./data/HN_.csv'=2830,
'./data/HQ_.csv'=9933,
'./data/HS_.csv'=139,
'./data/HT_.csv'=2529,
'./data/HV_.csv'=5736,
'./data/HX_.csv'=1263,
'./data/H_.csv'=996,
'./data/J__JZ_.csv'=16848,
'./data/K__KZ_.csv'=5247,
'./data/L__LT.csv'=11112,
'./data/PA_.csv'=8328,
'./data/PB_.csv'=202,
'./data/PC_.csv'=1372,
'./data/PD_.csv'=77,
'./data/PE_.csv'=1535,
'./data/PF_.csv'=277,
'./data/PG_.csv'=4402,
'./data/PH_.csv'=95,
'./data/PJ_.csv'=1663,
'./data/PK_.csv'=1880,
'./data/PL_.csv'=4580,
'./data/PM_.csv'=419,
'./data/PN_.csv'=19076,
'./data/PQ_.csv'=17337,
'./data/PR_.csv'=29557,
'./data/PS_.csv'=27502,
'./data/PT_.csv'=4435,
'./data/PZ_.csv'=1656,
'./data/P_.csv'=3390,
'./data/U__UH.csv'=2720,
'./data/V__VM_.csv'=301,
'./data/Z__ZA_.csv'=3343
)
  
cutoff <- cutoffs[[filename]]
# This like makes the cutoff for volumes to store in clemons
#print(data_tibble$modelC)
#print(cutoff)
data_tibble <- mutate(data_tibble,model=ifelse(modelC<cutoff,'1','0'))
#print(data_tibble$model)

# collapse all dupes into one entry
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumInhouseUses=sum(Item.Lifetime.Inhouse.Uses)) %>% ungroup()
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumCheckouts=sum(Item.Lifetime.Checkout.Corrected)) %>% ungroup()
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate(NumRenewals=sum(Item.Lifetime.Renewals)) %>% ungroup()
data_tibble <- data_tibble %>% group_by(Catalog.Id) %>% mutate( MODEL = ifelse( sum(as.integer(model))>=1,'1','0' ) ) %>% ungroup()


# make count of duplicate ids
data_tibble_uniques <- count(data_tibble,Catalog.Id)

# join dataframes together
data_tibble <- left_join(data_tibble,data_tibble_uniques)

# remove duplicate catalog ids
#data_tibble <- distinct(data_tibble,Catalog.Id,.keep_all=TRUE)

# add column for multi volume set
vol1 <-"\\sAbh.*$|\\sAbt.*$|\\san.*$|\\sband*$|\\sbd.*$|\\sBde.*$|\\sbk.*$|\\sbr.*$|\\sBuch*$|\\sc.*$|\\sch.*$|\\scis.*$|\\scuad.*$|\\scz.*$|\\sd.*$|\\sfasc.*$|\\sF.*$|\\sfol.*$|\\sg.*$|\\sHalbbd.*$|\\sHeft*$|\\siss.*$|\\sissue*$|\\sjaarg.*$|\\sJahrg.*$|\\skn.*$|\\skng.*$|\\sl.*$|\\sLfg.*$|\\sn.F.*$|\\snouv.*$|\\sn.r.*$|\\sno.*$|\\snr.*$|\\snu.*$|\\spt.*$|\\squad.*$|\\sReihe*$|\\sr.*$|\\sroc.*$|\\srocz.*$|\\ssb.*$|\\sser.*$|\\sses.*$|\\ssess.*$|\\sSdhft.*$|\\ssuppl.*$|\\ssv.*$|\\sT.*$|\\stbd.*$|\\steil*$|\\stomo*$|\\stome*$|\\sv.*$|\\svyd.*$|\\svyp.*$|\\swyd.*$|\\swydz.*$|\\syr.*$|\\szesz.*$"
vol2 <-"Abh.*$|Abt.*$|an.*$|v.*$|vyd.*$|vyp.*$|wyd.*$|wydz.*$|yr.*$|zesz.*$"
data_tibble <- mutate(data_tibble,MultiVolume=grepl(vol1,Item.Call.Number,ignore.case=TRUE))


# filter to only keep items for clemons (aka modelC==1)
#data_tibble <- filter(data_tibble, model == 1)


# select columns for liaisons
#data_tibble <- select(data_tibble,Item.Shelving.Id,Catalog.Id,Item.Barcode,Item.Call.Number,Item.Library.Code,Catalog.Title,Catalog.Author,Catalog.Pub.Year,Item.Created.Date,Item.Last.Checkout.Date,NumInhouseUses,NumCheckouts,NumRenewals,MultiVolume,modelC,MODEL)





# select columns for picklist
#data_tibble <- select(data_tibble,Item.Library.Code,Item.Shelving.Id,Item.Barcode,Catalog.Title,Catalog.Author,Catalog.Pub.Year,MultiVolume)

# sort on Item Shelving Id (aka callnumber order)
#data_tibble <- arrange(data_tibble,Item.Shelving.Id)

print('finish loop')
print(filename)


return(data_tibble)
}




