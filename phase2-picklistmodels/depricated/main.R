# main script
# lpa2a
# 2018-06-06
# drive program

# Load Necessary Functions
source("data_cleaner.R")
source("data_visualizer.R")
source("normalizer_modeler.R")

# Configure To run on desired data
filename <- "B.csv"
beta <- c(-1,-1,1,1,-1,1)
volumes <- 16483

# Turn the crank
tib <- data_cleaner(filename)
tib <- normalizer(tib)
tib <- S_model(beta,tib)

# Outputs: Visualizations and pick lists
data_visualizer(tib,filename)
write.csv(tail(tib,volumes),file = paste(filename,".clemons.csv",sep=""))
write.csv(head(tib,-volumes),file = paste(filename,".ivy.csv",sep=""))
