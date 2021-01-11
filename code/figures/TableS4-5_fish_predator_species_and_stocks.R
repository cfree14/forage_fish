
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

# Read diet prop data
diet_props_orig <- read.csv(file.path("data/merged_data/predator_diet_proportions_final.csv"), as.is=T)

# Read raw diet data
diets_orig <- read.csv("data/hilborn_etal_2017/hilborn_etal_2017_diet_information.csv", as.is=T)

# Read stock key
stock_key <- read.csv("tables/fish_predator_to_prey_key.csv", as.is=T)



# Calculate diet ranges
################################################################################

# Calculate diet stats
diet_stats <- diets_orig %>% 
  select(ocean, pred_comm_name, prey_comm_name, prop_diet_by_energy, prop_diet_by_wt, prop_diet_by_n, prop_diet_by_occur, prop_diet_by_index) %>% 
  gather(key="prop_type", value="prop", 4:ncol(.)) %>% 
  filter(!is.na(prop)) %>% 
  group_by(ocean, pred_comm_name, prey_comm_name) %>% 
  summarize(prop_avg=mean(prop),
            prod_med=median(prop),
            prop_min=min(prop),
            prop_max=max(prop)) %>% 
  ungroup() %>% 
  # Add diet id
  mutate(dietid=paste(pred_comm_name, ocean))

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

# Process diet data
table(diet_props_orig$prop_use_type)
diet_ref_stats <- diet_props_orig %>% 
  # Reduce
  filter(prop_use_type!="none") %>% 
  # Recode diet metric
  mutate(prop_use_type=recode(prop_use_type, 
                              "n"="count", 
                              "occur"="occurence",
                              "wt"="weight")) %>% 
  # Summarize stats
  group_by(dietid) %>% 
  summarize(nrefs=sum(nrefs),
            refs=paste(references, collapse=", "),
            metrics=paste(sort(unique(prop_use_type)), collapse=", ")) %>% 
  ungroup()

# Build data
# region, stockid, common_name (species), area, years, biomass_type, source
pred_stocks1 <- stock_key %>% 
  # Add diet id
  left_join(pred_stocks %>% select(stockid, dietid)) %>% 
  # Add diet composition reference stats
  left_join(diet_ref_stats, by="dietid") %>% 
  # Arrange
  select(region, stockid, name, areaname, 
         prey1, prey1_stocks, prey1_prop, 
         prey_impt, prey_add_stocks, prey_add_notes, prey_impt_prop, dietid, nrefs, refs, metrics,)

# Export data
write.csv(pred_stocks1, file=file.path(tabledir, "TableS5_fish_predator_stocks_all.csv"), row.names=F)


  



