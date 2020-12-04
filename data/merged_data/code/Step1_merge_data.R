

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir1 <- "data/hilborn_etal_2017"
datadir2 <- "data/new_data/"
datadir3 <- "data/cury_etal_2011"
datadir4 <- "data/pribolof_fur_seals"
outputdir <- "data/merged_data"

# Read Hilborn et al. (2017) data
taxa1 <- read.csv(paste(datadir1, "hilborn_etal_2017_taxa_info.csv", sep="/"), as.is=T)
pred_ts1 <- read.csv(paste(datadir1, "hilborn_etal_2017_predator_time_series.csv", sep="/"), as.is=T)
pred_stocks1 <- read.csv(paste(datadir1, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)
diet_props1 <- read.csv(paste(datadir1, "hilborn_etal_2017_diet_proportions.csv", sep="/"), as.is=T)
diet_info1 <- read.csv(paste(datadir1, "hilborn_etal_2017_diet_information.csv", sep="/"), as.is=T)

# Read new data
taxa2 <- read.csv(paste(datadir2, "taxanomic_key.csv", sep="/"), as.is=T)
pred_ts2 <- read.csv(paste(datadir2, "predator_time_series.csv", sep="/"), as.is=T)
pred_stocks2 <- read.csv(paste(datadir2, "predator_stocks.csv", sep="/"), as.is=T)
diet_props2 <- read.csv(paste(datadir2, "predator_diet_proportions.csv", sep="/"), as.is=T)
diet_info2 <- read.csv(paste(datadir2, "predator_diet_information.csv", sep="/"), as.is=T)

# Read Cury et al. (2011) data
taxa3 <- read.csv(file.path(datadir3, "cury_etal_2011_taxa.csv"), as.is=T)
pred_ts3 <- read.csv(file.path(datadir3, "cury_etal_2011_predator_time_series.csv"), as.is=T)
pred_stocks3 <- read.csv(file.path(datadir3, "cury_etal_2011_predator_populations.csv"), as.is=T)
prey_ts3 <- read.csv(file.path(datadir3, "cury_etal_2011_prey_time_series.csv"), as.is=T)
prey_stocks3 <- read.csv(file.path(datadir3, "cury_etal_2011_prey_populations.csv"), as.is=T)
diet_props3 <- read.csv(file.path(datadir3, "cury_etal_2011_diet_props.csv"), as.is=T)

# Read Pribolof Island Northern fur seal info
taxa4 <- read.csv(file.path(datadir4, "pribolof_taxa_info.csv"), as.is=T)
pred_ts4 <- read.csv(file.path(datadir4, "pribolof_fur_seal_abundance.csv"), as.is=T)
pred_stocks4 <- read.csv(file.path(datadir4, "pribolof_fur_seal_stocks.csv"), as.is=T)
diet_props4 <- read.csv(file.path(datadir4, "pribolof_fur_seal_diet_props.csv"), as.is=T)


################################################################################
# Merge taxanomic keys
################################################################################

# Inspect colnames
colnames(taxa1)
colnames(taxa2)
colnames(taxa3)
colnames(taxa4)

# Format taxanomic key
taxa <- unique(rbind.fill(taxa1, taxa2, taxa3, taxa4)) %>% 
  arrange(type, species_sub)

# Any taxa requiring corrections?
# If this reports anything but 0, then the diet ids are messed up!
taxa$species_sub[duplicated(taxa$species_sub)]
taxa$comm_name[duplicated(taxa$comm_name)]


################################################################################
# Merge predator diet info
################################################################################

# Inspect colnames
colnames(diet_info1)
colnames(diet_info2)


################################################################################
# Merge predator diet proportions
################################################################################

# Inspect colnames
colnames(diet_props1)
colnames(diet_props2)
colnames(diet_props3)
colnames(diet_props4)

# Columns for merged file
# source, dietid, pred_comm, region, prey_comm
# prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type

# Format Hilborn et al. (2017) data
diet_props1f <- diet_props1 %>% 
  rename(pred_comm=pred_comm_name, region=ocean, prey_comm=prey_comm_name, references=refs) %>% 
  mutate(source="Hilborn et al. (2017)",
         dietid=paste(pred_comm, region),
         prop_model=NA, prop_unk=NA) %>% 
  select(source, dietid, pred_comm, region, prey_comm, nrefs, references, 
         prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type)

# Format Free et al. (in prep) data
diet_props2f <- diet_props2 %>% 
  rename(dietid=diet_id) %>% 
  mutate(source="Free et al. (in prep)", 
         prop_energy=NA) %>% 
  select(source, dietid, pred_comm, region, prey_comm,  nrefs, references, 
         prop_wt, prop_n, prop_energy, prop_occur, prop_model, prop_unk, prop_use, prop_use_type)

# Format Cury et al. 2011 data
diet_props3f <- diet_props3 %>% 
  mutate(nrefs=1,
         references=source) %>% 
  select(source, dietid, pred_comm, region, prey_comm, nrefs, references, everything())

# Format Pribolof Island Northern fur seal data
diet_props4f <- diet_props4 %>% 
  rename(source=reference, region=area, prop_occur=diet_prop) %>% 
  mutate(nrefs=1,
         references=source,
         prop_occur=prop_occur/100,
         prop_use=prop_occur,
         prop_use_type="occurence") %>% 
  select(source, dietid, pred_comm, region, prey_comm, nrefs, references,
         prop_occur, prop_use, prop_use_type)

# Merge data
diet_props <- bind_rows(diet_props1f, diet_props2f, diet_props3f, diet_props4f)

# Inspect diet prop ranges

# Check completeness
freeR::complete(diet_props)



################################################################################
# Merge predator stock info
################################################################################

# Inspect colnames
colnames(pred_stocks1)
colnames(pred_stocks2)
colnames(pred_stocks3)
colnames(pred_stocks4)

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

# Format Cury et al. (2011) data
pred_stocks3f <- pred_stocks3 %>% 
  select(-c(type, class, order, family)) %>% 
  rename(pred_comm=comm_name, pred_species=species_sub)

# Format Pribolof data
pred_stocks4f <- pred_stocks4 %>% 
  rename(pred_comm=comm_name, pred_species=species)

# Merge data
pred_stocks <- rbind.fill(pred_stocks1f, pred_stocks2f, pred_stocks3f, pred_stocks4f) %>% 
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
colnames(pred_ts3)
colnames(pred_ts4)

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

# Format Cury et al. (2011) data
pred_ts3f <- pred_ts3 %>%
  mutate(source="Cury et al. (2011)",
         catch=NA, notes=NA) %>% 
  select(source, stocklong, reference, year, n, n_units, catch, notes)

# Format Pribolof data
pred_ts4f <- pred_ts4 %>%
  rename(n=abundance,
         n_units=units) %>% 
  mutate(source="Towell et al. (2016)", 
         stocklong=paste(comm_name, island), 
         catch=NA, notes=NA) %>% 
  select(-c(island, species, comm_name))

# Merge data
pred_ts <- rbind.fill(pred_ts1f, pred_ts2f, pred_ts3f, pred_ts4f)

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





