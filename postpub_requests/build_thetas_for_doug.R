
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
outputdir <- "output"
tabledir <- "tables"
plotdir <- "figures"

# Read data
data_orig <- read.csv(file.path(outputdir, "model_results.csv"))

# Load data
load(file.path(datadir, "data_composite_final_sst.Rdata"))
rm(data)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to estimates of interest
  filter(dataset=="composite" & framework=="fixed" & covariate=="prey1") %>% 
  # Simpolify columns
  select(type, stockid, betaT, betaT_lo, betaT_hi, betaT_inf) %>% 
  # Rename columns
  rename(theta=betaT, theta_lo=betaT_lo, theta_hi=betaT_hi, theta_inf=betaT_inf) %>% 
  # Add stock info
  left_join(stocks %>% select(stockid, stocklong, region, area, location, species, comm_name), by="stockid") %>% 
  # Arrange
  select(stockid, stocklong, region:comm_name, type, theta:theta_inf, everything())

# Export data
write.csv(data, file="postpub_requests/data/primary_prey_fixed_effects_for_doug.csv", row.names=F)

