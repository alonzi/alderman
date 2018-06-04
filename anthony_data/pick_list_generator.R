# data summary visualizations
# lpa2a
# 2018-06-04
# pick list generator

library(tidyverse)

# Usage
#filename <- "B.csv"
#tib <- data_cleaner(filename)
#data_visualizer(tib,filename)

#x0 - catalog.pub.year -- compute number of days -- days
#x1 - item.create.date -- compute number of days -- days
#x2 - item.lifetime.checkout -- num
#x3 - item.lifetime.renewals -- num
#x4 - days_since_last_checkout -- days
#x5 - checkouts_per_day -- num / day

tib <- mutate(tib,x0=(365*(2018-tib$Catalog.Pub.Year)-mean(365*(2018-tib$Catalog.Pub.Year)))/sqrt(var(365*(2018-tib$Catalog.Pub.Year))))
tib <- mutate(tib,x1=(today()-mdy(tib$Item.Created.Date)-mean(today()-mdy(tib$Item.Created.Date)))/sqrt(var(today()-mdy(tib$Item.Created.Date))))
tib <- mutate(tib,x2=(tib$Item.Lifetime.Checkout-mean(tib$Item.Lifetime.Checkout))/sqrt(var(tib$Item.Lifetime.Checkout)))
tib <- mutate(tib,x3=(tib$Item.Lifetime.Renewals-mean(tib$Item.Lifetime.Renewals))/sqrt(var(tib$Item.Lifetime.Renewals)))
tib <- mutate(tib,x4=(tib$days_since_last_checkout-mean(tib$days_since_last_checkout))/sqrt(var(tib$days_since_last_checkout)))
tib <- mutate(tib,x5=(tib$checkouts_per_day-mean(tib$checkouts_per_day))/sqrt(var(tib$checkouts_per_day)))

