
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(freeR)
library(tidyverse)

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
  # Format regions
  mutate(region=recode(region, 
                       "Benguela Current"="South Africa",
                       "California Current"="US/Canada West Coast",
                       "North Sea"="Europe",
                       "Norwegian Sea"="Europe",
                       "US Alaska"="US/Canada West Coast",
                       "USA/Canada East"="US/Canada East Coast",
                       "USA/Canada West"="US/Canada West Coast"),
         species=recode(species, "Ammodytes spp."="Ammodytes marinus")) %>% 
  # Group by regions
  group_by(region, comm_name, species) %>% 
  summarize(nstocks=n()) %>% 
  ungroup() %>% 
  # Add name
  mutate(name_format=paste0(comm_name, " (", species, ")")) %>% 
  # Arrange
  select(region, name_format, nstocks) %>% 
  arrange(region, desc(nstocks)) %>% 
  # Remove New Zealand and rockfish (not used in analysis)
  filter(region!="New Zealand" & name_format!="Rockfish spp. (Sebastes spp.)")

# Export data
write.csv(prey, file=file.path(tabledir, "TableS2_prey_stock_summary.csv"), row.names=F)
  

# Table S3. Prey stocks
################################################################################

# Build data
# region, stockid, common_name (species), area, years, biomass_type, source
prey_stocks1 <- prey_stocks %>% 
  # Add name
  mutate(name = paste0(comm_name, " (", species, ")")) %>% 
  # Clean biomass type
  mutate(biomass_type=toupper(biomass_type),
         biomass_type=ifelse(is.na(biomass_type), "Index", biomass_type)) %>%
  # Clean region
  mutate(region=recode(region, 
                       "Benguela Current"="South Africa",
                       "California Current"="US/Canada West Coast",
                       "North Sea"="Europe",
                       "Norwegian Sea"="Europe",
                       "US Alaska"="US/Canada West Coast",
                       "USA/Canada East"="US/Canada East Coast",
                       "USA/Canada West"="US/Canada West Coast")) %>% 
  # Clean source
  mutate(source=ifelse(!is.na(assessid), "RAM Legacy Database", assessorid),
         source=gsub("et al ", "et al. ", source)) %>% 
  # Arrange data
  select(region, stockid, name, areaname, biomass_type, yrs, nyr, source) %>% 
  arrange(region, name)

# Export data
write.csv(prey_stocks1, file=file.path(tabledir, "TableS3_prey_stocks_all.csv"), row.names=F)


