
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(reshape2)
library(tidyverse)

# Directories
datadir <- "data/hilborn_etal_2017/"
plotdir <- "figures"

# Read data
diets <- read.csv(paste(datadir, "hilborn_etal_2017_diet_information.csv", sep="/"), as.is=T)

# Calculate range
################################################################################

# Calculate diet stats
diet_stats <- diets %>% 
  select(ocean, pred_comm_name, prey_comm_name, prop_diet_by_energy, prop_diet_by_wt, prop_diet_by_n, prop_diet_by_occur, prop_diet_by_index) %>% 
  gather(key="prop_type", value="prop", 4:ncol(.)) %>% 
  filter(!is.na(prop)) %>% 
  group_by(ocean, pred_comm_name, prey_comm_name) %>% 
  summarize(prop_avg=mean(prop),
            prod_med=median(prop),
            prop_min=min(prop),
            prop_max=max(prop)) %>% 
  ungroup()
