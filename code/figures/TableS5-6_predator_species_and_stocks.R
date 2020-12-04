
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)
library(freeR)

# Directories
datadir <- "data"
tabledir <- "tables"

# Read data
load(file.path(datadir, "fish_predator_data.Rdata"))


# Table S5. Prey stock summary
################################################################################

# Format data
table(pred_stocks$region)
pred <- pred_stocks %>% 
  group_by(region, comm_name, species) %>% 
  summarize(nstocks=n()) %>% 
  ungroup() %>% 
  mutate(region=recode(region, 
                       "Atlantic Ocean"="USA/Canada East Coast",   
                       "Pacific Ocean"="USA/Canada West Coast",     
                       "USA/Canada East"="USA/Canada East Coast",   
                       "USA/Canada West"="USA/Canada West Coast")) %>% 
  mutate(name_format=paste0(comm_name, " (", species, ")")) %>% 
  select(region, name_format, comm_name, species, nstocks) %>% 
  arrange(region, name_format)

# Export data
write.csv(pred, file=file.path(tabledir, "TableS4_fish_predator_stock_summary.csv"), row.names=F)
  

# Table S6. Predator stocks
################################################################################

# Build data
# region, stockid, common_name (species), area, years, biomass_type, source
pred_stocks1 <- pred_stocks %>% 
  mutate(name= paste0(comm_name, " (", species, ")")) %>%
  select(region, stockid, name, areaname, nprey, prey1, prey1_prop, prey_impt, prey_impt_prop) %>% 
  mutate(region=recode(region, 
                       "Atlantic Ocean"="USA/Canada East Coast",   
                       "Pacific Ocean"="USA/Canada West Coast",     
                       "USA/Canada East"="USA/Canada East Coast",   
                       "USA/Canada West"="USA/Canada West Coast")) %>% 
  arrange(region, name)

# Export data
write.csv(pred_stocks1, file=file.path(tabledir, "TableS6_fish_predator_stocks_all.csv"), row.names=F)


  



