# data summary visualizations
# lpa2a
# 2018-06-04
# pick list generator

library(tidyverse)

# Usage
filename <- "B.csv"
tib <- data_cleaner(filename)
data_visualizer(tib,filename)
### Normalize Features
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


# Model Function
flat_model <- function(beta,tib){
    tib <- mutate(tib,S_flat=1/(1+exp(-(beta[1]*as.numeric(x0)+beta[2]*as.numeric(x1)+beta[3]*as.numeric(x2)+beta[4]*as.numeric(x3)+beta[5]*as.numeric(x4)+beta[6]*as.numeric(x5)))))
    return(tib)
}


#Usage
beta <- c(-1,-1,1,1,-1,1)
tib <- flat_model(beta,tib)
# plot S flat distribution
ggplot(data=tib, aes(S_flat)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))

# B target = 16483 volumes
tib <-arrange(tib,S_flat)
ggplot(data=tail(tib,16483), aes(S_flat)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0,1))


B_pick_list <- tail(tib,16483)
write.csv(tail(tib,16483),file = "B_pick_list.csv")

