

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir1 <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017"
datadir2 <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/new_data/"
outputdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/merged_data"

# Read Hilborn et al. (2017) data
taxa1 <- read.csv(paste(datadir1, "hilborn_etal_2017_taxa_info.csv", sep="/"), as.is=T)
pred_ts1 <- read.csv(paste(datadir1, "hilborn_etal_2017_predator_time_series.csv", sep="/"), as.is=T)
pred_stocks1 <- read.csv(paste(datadir1, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)
diet_props1 <- read.csv(paste(datadir1, "hilborn_etal_2017_diet_proportions.csv", sep="/"), as.is=T)

# Read new data
taxa2 <- read.csv(paste(datadir2, "taxanomic_key.csv", sep="/"), as.is=T)
pred_ts2 <- read.csv(paste(datadir2, "predator_time_series.csv", sep="/"), as.is=T)
pred_stocks2 <- read.csv(paste(datadir2, "predator_stocks.csv", sep="/"), as.is=T)
diet_props2 <- read.csv(paste(datadir2, "predator_diet_proportions.csv", sep="/"), as.is=T)


################################################################################
# Merge taxanomic keys
################################################################################

# Inspect colnames
colnames(taxa1)
colnames(taxa2)

# Format taxanomic key
taxa <- unique(rbind(taxa1, taxa2)) %>% 
  arrange(type, species_sub)

# Any taxa requiring corrections?
# If this reports anything but 0, then the diet ids are messed up!
taxa$species_sub[duplicated(taxa$species_sub)]
taxa$comm_name[duplicated(taxa$comm_name)]


################################################################################
# Merge predator diet proportions
################################################################################

# Inspect colnames
colnames(diet_props1)
colnames(diet_props2)

# Columns for merged file
# source, dietid, pred_comm, region, prey_comm
# prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type

# Format Hilborn et al. (2017) data
diet_props1f <- diet_props1 %>% 
  rename(pred_comm=pred_comm_name, region=ocean, prey_comm=prey_comm_name) %>% 
  mutate(source="Hilborn et al. (2017)",
         dietid=paste(pred_comm, region),
         prop_model=NA, prop_unk=NA) %>% 
  select(source, dietid, pred_comm, region, prey_comm,
         prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type)

# Format Free et al. (in prep) data
diet_props2f <- diet_props2 %>% 
  rename(dietid=diet_id) %>% 
  mutate(source="Free et al. (in prep)", 
         prop_energy=NA) %>% 
  select(source, dietid, pred_comm, region, prey_comm,
         prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type)

# Merge data
diet_props <- rbind(diet_props1f, diet_props2f)

# Check completeness and confirm unique id
apply(diet_props, 2, function(x) sum(is.na(x)))


################################################################################
# Merge predator stock info
################################################################################

# Inspect colnames
colnames(pred_stocks1)
colnames(pred_stocks2)

# Columns for merged file
# source, stocklong, stockid, dietid, region, area, location, 
# type, class, order, family, species, comm_name, reference, n_units, n_yr, diet_yn, diet_prop

# Format Hilborn et al. (2017) data
pred_stocks1f <- pred_stocks1 %>% 
  rename(location=area, area=region, region=ocean, pred_species=species, pred_comm=comm_name, n_yr=nyr) %>% 
  mutate(source="Hilborn et al. (2017)",
         dietid=paste(pred_comm, region)) %>% 
  select(source, stocklong, stockid, dietid, region, area, location, pred_comm, pred_species, reference, n_units, n_yr)

# Format Free et al. (in prep) data
pred_stocks2f <- pred_stocks2 %>% 
  mutate(source="Free et al. (in prep)", 
         stockid=NA,
         dietid=paste(pred_comm, region)) %>% 
  select(source, stocklong, stockid, dietid, region, area, location, pred_comm, pred_species, reference, n_units, n_yr)

# Merge data
pred_stocks <- rbind(pred_stocks1f, pred_stocks2f) %>% 
  left_join(select(taxa, type, class, order, family, species_sub), by=c("pred_species"="species_sub")) %>% 
  mutate(diet_yn=ifelse(dietid%in%diet_props$dietid, "Y", "N"),
         diet_prop=sapply(dietid, function(x) sum(diet_props$prop_use[diet_props$dietid==x], na.rm=T))) %>% 
  select(source, stocklong, stockid, dietid, region, area, location,
         type, class, order, family, pred_species, pred_comm, reference, n_units, n_yr, diet_yn, diet_prop) %>% 
  rename(species=pred_species, comm_name=pred_comm, pred_type=type)

# Check completeness and confirm unique id
anyDuplicated(pred_stocks$stocklong)
apply(pred_stocks, 2, function(x) sum(is.na(x)))


################################################################################
# Merge predator time series
################################################################################

# Inspect colnames
colnames(pred_ts1)
colnames(pred_ts2)

# Columns for merged file
# source, stocklong, reference, year, n, n_units, catch, notes

# Format Hilborn et al. (2017) data
pred_ts1f <- pred_ts1 %>% 
  mutate(source="Hilborn et al. (2017)",
         notes=NA) %>% 
  select(source, stocklong, reference, year, n, n_units, catch, notes)

# Format Free et al. (in prep) data
pred_ts2f <- pred_ts2 %>% 
  mutate(source="Free et al. (in prep)",
         catch=NA) %>% 
  select(source, stocklong, reference, year, n, n_units, catch, notes)

# Merge data
pred_ts <- rbind(pred_ts1f, pred_ts2f)

# Check completeness
apply(pred_ts, 2, function(x) sum(is.na(x)))


################################################################################
# Export data
################################################################################

# Export data
write.csv(diet_props, paste(outputdir, "predator_diet_proportions_final.csv", sep="/"), row.names=F)
write.csv(pred_stocks, paste(outputdir, "predator_stocks_final.csv", sep="/"), row.names=F)
write.csv(pred_ts, paste(outputdir, "predator_time_series_final.csv", sep="/"), row.names=F)
write.csv(taxa, paste(outputdir, "taxanomic_info_final.csv", sep="/"), row.names=F)





