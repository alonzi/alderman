# liaison list generator script
# lpa2a
# 2018-07-19
# usage: bash$ Rscript src/generate_liaison_lists.R

# Load Necessary Functions
source("./src/data_cleaner.R")

# Configure To run on desired data
datapath <- "./dat"
input_files <- list.files(datapath)
input_files <- paste(datapath,input_files,sep="/")
output_files <- paste("./liaison_review/x",list.files(datapath),sep="")

#input_files <- "./dat/BR_.csv" # use for testing, comment out for production
#output_files <- "./liaison_review/xBR_.csv"

#output_files <- gsub('.{3}$', '', output_files)
#output_files <- paste(output_files,"xlsx",sep="")

# Turn the crank
tibbles <- lapply(input_files,data_cleaner)

# make output files
for (i in 1:length(output_files)){
  print(paste("writing file",output_files[i]))
  write.csv(tibbles[i],file = output_files[i])
}
