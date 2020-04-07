
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data"
tabledir <- "tables"

# Read data
load(file.path(datadir, "prey_data.Rdata"))


# Table S2. Prey stock summary
################################################################################

# Prey species
sort(unique(prey_stocks$species))

# Format data
prey <- prey_stocks %>% 
  group_by(region, comm_name, species) %>% 
  summarize(nstocks=n()) %>% 
  mutate(name_format=paste0(comm_name, " (", species, ")")) %>% 
  select(region, name_format, comm_name, species, nstocks)

# Export data
write.csv(prey, file=file.path(tabledir, "TableS2_prey_stock_summary.csv"), row.names=F)
  

# Table S3. Prey stocks
################################################################################

# Build data
# region, stockid, common_name (species), area, years, biomass_type, source
prey_stocks1 <- prey_stocks %>% 
  mutate(biomass_type=toupper(biomass_type),
         name= paste0(comm_name, " (", species, ")")) %>%
  select(region, stockid, name, areaname, biomass_type, yrs, nyr) %>% 
  arrange(region, name)

# Export data
write.csv(prey_stocks1, file=file.path(tabledir, "TableS3_prey_stocks_all.csv"), row.names=F)


