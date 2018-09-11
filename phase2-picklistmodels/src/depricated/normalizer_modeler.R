# data summary visualizations
# lpa2a
# 2018-06-04
# pick list generator

library(tidyverse)



### Normalize Features
#x0 - catalog.pub.year -- compute number of days -- days
#x1 - item.create.date -- compute number of days -- days
#x2 - item.lifetime.checkout -- num
#x3 - item.lifetime.renewals -- num
#x4 - days_since_last_checkout -- days
#x5 - checkouts_per_day -- num / day
normalizer <- function(tib){
  
#  tib %>% 
#    group_by(Catalog.Id) %>% 
#    filter(max(row_number()) > 1) %>%
#    ungroup()
  
  
tib <- mutate(tib,x0=(365*(2018-tib$Catalog.Pub.Year)-mean(365*(2018-tib$Catalog.Pub.Year)))/sqrt(var(365*(2018-tib$Catalog.Pub.Year))))
tib <- mutate(tib,x1=(today()-mdy(tib$Item.Created.Date)-mean(today()-mdy(tib$Item.Created.Date)))/sqrt(var(today()-mdy(tib$Item.Created.Date))))
tib <- mutate(tib,x2=(tib$Item.Lifetime.Checkout-mean(tib$Item.Lifetime.Checkout))/sqrt(var(tib$Item.Lifetime.Checkout)))
tib <- mutate(tib,x3=(tib$Item.Lifetime.Renewals-mean(tib$Item.Lifetime.Renewals))/sqrt(var(tib$Item.Lifetime.Renewals)))
tib <- mutate(tib,x4=(tib$days_since_last_checkout-mean(tib$days_since_last_checkout))/sqrt(var(tib$days_since_last_checkout)))
tib <- mutate(tib,x5=(tib$checkouts_per_day-mean(tib$checkouts_per_day))/sqrt(var(tib$checkouts_per_day)))
return(tib)
}

# Model Function
model <- function(tib){
  beta <- c(-1,-1,1,1,-1,1) # random betas
  tib <- mutate(tib,scoreA=1/(1+exp(-(beta[1]*as.numeric(x0)+beta[2]*as.numeric(x1)+beta[3]*as.numeric(x2)+beta[4]*as.numeric(x3)+beta[5]*as.numeric(x4)+beta[6]*as.numeric(x5)))))
  beta <- c(-1,-1,1,1,-1,1) # random volumes
  tib <- mutate(tib,scoreB=2.*(runif(1)-0.5))
  beta <- c(-1,-1,1,1,-1,1) # flat
  tib <- mutate(tib,scoreC=1/(1+exp(-(beta[1]*as.numeric(x0)+beta[2]*as.numeric(x1)+beta[3]*as.numeric(x2)+beta[4]*as.numeric(x3)+beta[5]*as.numeric(x4)+beta[6]*as.numeric(x5)))))
  
#  tib <- arrange(tib,S_flat) # sort in descending order
    
  return(tib)
}












