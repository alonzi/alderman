# liaison list generator script
# lpa2a
# 2018-07-19
# usage: bash$ Rscript src/generate_liaison_lists.R

# Load Necessary Functions
source("./src/data_cleaner.R")

# Configure To run on desired data
datapath <- "./data"
input_files <- list.files(datapath)
input_files <- paste(datapath,input_files,sep="/")
output_files <- paste("./picklists2/x",list.files(datapath),sep="")

# Turn the crank
tibbles <- lapply(input_files,data_cleaner)

# make output files
for (i in 1:length(output_files)){
  print(paste("writing file",output_files[i]))
  write.csv(tibbles[i],file = output_files[i])
}
