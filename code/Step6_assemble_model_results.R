
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data"
outputdir <- "output"

# Which results to prepare?
# sst or prey1
type <- "sst"
if(type=="prey1"){
  infile <- "pella_best_fixed_prey1.Rdata"
  outfile <- "pella_best_fixed_prey1_results.csv"
}
if(type=="sst"){
  infile <- "pella_best_fixed_sst.Rdata"
  outfile <- "pella_best_fixed_sst_results.csv"
}

# Read model output
load(file.path(outputdir, infile))

# Read model data
load(file.path(datadir, "data_final_sst.Rdata"))


# Build data
################################################################################

# Build data
output <- stocks %>% 
  left_join(results, by="stockid") %>% 
  filter(!is.na(r))


# Export data
################################################################################

# Export data
write.csv(output, file=file.path(outputdir, outfile), row.names=F)

