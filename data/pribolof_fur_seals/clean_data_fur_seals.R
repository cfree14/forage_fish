

# Clean
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(ggplot2)
library(rio)

# Directories
outdir <- "data/pribolof_fur_seals"
datadir <- "data/pribolof_fur_seals/original"

# Read data
data_orig <- import(file.path(datadir, "Northern Fur Seal Diet and Abundance.xlsx"), which=2)
diet_orig <- import(file.path(datadir, "Northern Fur Seal Diet and Abundance.xlsx"), which=1)

# Format abundance data
################################################################################

# Format data
data1 <- data_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(comm_name=`predator common`, species=`predator scientific`, age_class=`age class`) %>% 
  mutate(year=round(year),
         abundance=round(as.numeric(abundance)),
         comm_name=freeR::sentcase(comm_name),
         island=revalue(island, c("st paul"="St. Paul", 
                                  "st george"="St. George")))

# Inspect data
stats <- data1 %>% 
  group_by(island, age_class) %>% 
  summarize(nyr=n(),
            yr_dups=anyDuplicated(year),
            yr1=min(year),
            yr2=max(year),
            yr_check=length(yr1:yr2)==nyr)

# Format data again
data2 <- data1 %>%
  filter(age_class%in%c("idle, class 2 and 5", "harem, class 3")) %>% 
  group_by(comm_name, species, island, reference, year) %>% 
  summarize(abundance=sum(abundance)) %>% 
  mutate(units="# of males (harem males + idle males)")

# Stocks
stocks <- data2 %>% 
  filter(!is.na(abundance)) %>% 
  group_by(comm_name, species, island, reference, units) %>% 
  summarize(n_yr=n()) %>% 
  mutate(source="Towell et al. (2016)", 
         stocklong=paste(comm_name, island),
         region="Bering Sea",
         area="Pribolof Islands",
         dietid=paste(comm_name, area)) %>% 
  rename(location=island, n_units=units) %>% 
  select(source, stocklong, dietid, region, area, location, comm_name, species,
         reference, n_units, n_yr)


# Format diet data
################################################################################

# Format diet data
diet <- diet_orig %>% 
  setNames(gsub(" ", "_", tolower(colnames(.)))) %>% 
  rename(pred_comm=predator_common, pred_species=predator_scientific, 
         prey_comm=prey_common, prey_species=prey_scientific, 
         units=units_of_diet, diet_prop=`%_in_diet`, area=island) %>% 
  group_by(pred_comm, pred_species, prey_comm, prey_species, area, reference, units) %>% 
  summarize(diet_prop=mean(diet_prop)) %>% 
  ungroup() %>% 
  mutate(area="Pribolof Islands",
         pred_comm=sentcase(pred_comm),
         dietid=paste(pred_comm, area),
         prey_comm=sentcase(prey_comm),
         prey_comm=revalue(prey_comm, c("Squid"="Armhook squid")),
         prey_species=revalue(prey_species, c("Gonatidae"="Gonatidae spp.")))

# Export data
################################################################################

# Export
write.csv(stocks, file.path(outdir, "pribolof_fur_seal_stocks.csv"), row.names=F)
write.csv(data2, file.path(outdir, "pribolof_fur_seal_abundance.csv"), row.names=F)
write.csv(diet, file.path(outdir, "pribolof_fur_seal_diet_props.csv"), row.names=F)





