
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
# random or fixed
type <- "prey1"
type1 <- "fixed"
if(type=="prey1"){
  if(type1=="fixed"){
    infile <- "pella_best_fixed_prey1.Rdata"
    outfile <- "pella_best_fixed_prey1_results.csv"
  }else{
    infile <- "pella_best_random_prey1.Rdata"
    outfile <- "pella_best_random_prey1_results.csv"
  }
}
if(type=="sst"){
  if(type1=="fixed"){
    infile <- "pella_best_fixed_sst.Rdata"
    outfile <- "pella_best_fixed_sst_results.csv"
  }else{
    infile <- "pella_best_random_sst.Rdata"
    outfile <- "pella_best_random_sst_results.csv"
  }
}

# Read model output
load(file.path(outputdir, infile))

# Read model data
load(file.path(datadir, "data_final_sst.Rdata"))


# Build data
################################################################################

# Build data
if(type1=="fixed"){
  output <- stocks %>% 
    left_join(results, by="stockid") %>% 
    filter(!is.na(r))
}else{
  output <- stocks %>% 
    left_join(results[[1]], by="stockid") %>% 
    filter(!is.na(r))
}

# Export data
################################################################################

# Export data
write.csv(output, file=file.path(outputdir, outfile), row.names=F)

