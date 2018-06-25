

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/merged_data"
tabledir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/tables"
  
# Read data
preds <- read.csv(paste(datadir, "predator_stocks_final.csv", sep="/"), as.is=T)


# Summarize data
################################################################################

# Summarize data
preds1 <- preds %>%
  filter(n_yr>=20) %>% 
  group_by(region, pred_type, comm_name, species) %>% 
  summarize(nstocks=n()) %>% 
  mutate(pred=paste0(comm_name, " (", species, ")")) %>% 
  ungroup() %>% 
  select(region, pred_type, pred, nstocks)


# Export data
################################################################################

# Export data
write.csv(preds1, paste(tabledir, "Table1_predator_sample.csv", sep="/"))

