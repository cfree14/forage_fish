
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data"
outputdir <- "postpub_requests/data"

# Read data
load(paste(inputdir, "data_composite_final_sst.Rdata", sep="/"))
data_orig <- data; stocks_orig <- stocks
rm(data, stocks)


# Setup
################################################################################

# Problem stocks
problem_stocks <- c("ELETERSDB", "SEALIONSCBpup", # production unrelated to abundance
                    "PERPELPERU614S", "PERBOOPERU614S", # enormous influences
                    "COMGUISHETALL", "HUMPBACKCAOR") # enormous influence SDs

# Remove problem stocks
data <- data_orig %>% 
  filter(!stockid %in% problem_stocks)
stocks <- stocks_orig %>% 
  filter(stockid %in% data$stockid)

# Check
n_distinct(data$stockid)
n_distinct(stocks$stockid)

# Export
write.csv(data, file=file.path(outputdir, "Free_etal_2021_predator_prey_sst_time_series.csv"), row.names=F)
write.csv(stocks, file=file.path(outputdir, "Free_etal_2021_predator_populations.csv"), row.names=F)
