
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
load(file.path(datadir, "nonfish_predator_data.Rdata"))
stock_meta <- read.csv("tables/bird_mammal_predator_populations_metadata.csv", as.is=T)

# Read stock key
stock_key <- read.csv("tables/nonfish_pred_key_with_stockids.csv", as.is=T)

# Read diet prop data
diet_props_orig <- read.csv(file.path("data/merged_data/predator_diet_proportions_final.csv"), as.is=T)

# Read raw diet data
diets_orig <- read.csv("data/hilborn_etal_2017/hilborn_etal_2017_diet_information.csv", as.is=T)


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
  mutate(region=recode(ocean, ))
  mutate(dietid=paste(pred_comm_name, ocean))


# Table S6. Nonfish Predator stocks
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

# Build table
data <- stock_key %>% 
  # Add abundance reference, type, and untis
  left_join(stock_meta %>% select(stockid, reference, n_units, dietid)) %>% 
  # Add diet composition reference stats
  left_join(diet_ref_stats, by="dietid")

# Export table
write.csv(data, file=file.path(tabledir, "TableS6_nonfish_predator_stocks.csv"), row.names=F)
  



