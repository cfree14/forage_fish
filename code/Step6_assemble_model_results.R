
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

# Read model output
load(file.path(outputdir, "pella0.55_fixed_prey1.Rdata"))

# Read model data
load(file.path(datadir, "data_final.Rdata"))


# Build data
################################################################################

# Build data
output <- stocks %>% 
  left_join(results, by="stockid") %>% 
  filter(!is.na(r))


# Export data
################################################################################

# Export data
write.csv(output, file=file.path(outputdir, "pella0.55_fixed_prey1_results.csv"), row.names=F)

