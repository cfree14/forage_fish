
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/new_data/"

# Read data
diets <- read.csv(paste(datadir, "predator_diet_information.csv", sep="/"), as.is=T)


# Summarize data
################################################################################

# Calculate diet proportions
props <- diets %>% 
  group_by(diet_id, pred_comm, region, prey_comm, diet_prop_units) %>% 
  summarize(prop=mean(diet_prop, na.rm=T)) %>% 
  mutate(diet_prop_units=ifelse(is.na(diet_prop_units), "unknown", gsub(" ", "_", diet_prop_units)))
props$prop[is.nan(props$prop)] <- NA
  
# Reshape diet proportions
props1 <- dcast(props, diet_id + pred_comm + region + prey_comm ~ diet_prop_units, value.var="prop")

# Rearrange and add prop to use columns
# weight > number > occur > model > unknown
props2 <- props1 %>% 
  select(diet_id, pred_comm, region, prey_comm,
         by_weight, by_number, by_freq_of_occurence, by_ecosystem_model, unknown) %>% 
  rename(prop_wt=by_weight, prop_n=by_number, prop_occur=by_freq_of_occurence, 
         prop_model=by_ecosystem_model, prop_unk=unknown) %>% 
  mutate(prop_use=ifelse(!is.na(prop_wt), prop_wt, 
                         ifelse(!is.na(prop_n), prop_n, 
                                ifelse(!is.na(prop_occur), prop_occur,
                                       ifelse(!is.na(prop_model), prop_model, 
                                              ifelse(!is.na(prop_unk), prop_unk, NA))))),
         prop_use_type=ifelse(!is.na(prop_wt), "weight", 
                              ifelse(!is.na(prop_n), "count", 
                                     ifelse(!is.na(prop_occur), "occurence",
                                            ifelse(!is.na(prop_model), "model", 
                                                   ifelse(!is.na(prop_unk), "unknown", "none"))))))

# Export data
write.csv(props2, paste(datadir, "predator_diet_proportions.csv", sep="/"), row.names=F)




