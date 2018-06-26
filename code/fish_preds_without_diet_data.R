

# Many RAM common names are wrong so their diet ids are wrong.
# Fix diet ids and many stocks will get eliminated
# Eliminate non-predators (definitely the forage species, shrimp, etc)


# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data/merged_data"

# Read diet data
diet_orig <- read.csv(file.path(datadir, "predator_diet_proportions_final.csv"), as.is=T)
taxa <- read.csv(file.path(datadir, "taxanomic_info_final.csv"), as.is=T)

# Read RAM Legacy Database (v4.40)
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.40 (6-4-18)/DB Files With Assessment Data/DBdata.RData")


# Format data
################################################################################


# Modfiy taxa
taxa <- taxa %>% 
  mutate(species_sub=revalue(species_sub, c("Clupea pallasii pallasii"="Clupea pallasii")))

# Add scientific names to diet data
diets <- diet_orig %>%
  # Add predator sci name
  left_join(select(taxa, comm_name, species), by=c("pred_comm"="comm_name")) %>% 
  rename(pred_spp=species) %>% 
  mutate(pred_spp=ifelse(pred_comm=="Pacific bonito", "Sarda chiliensis", pred_spp),
         pred_spp=ifelse(pred_comm=="Common murre", "Uria aalge", pred_spp)) %>% 
  # Add prey sci name
  left_join(select(taxa, comm_name, species), by=c("prey_comm"="comm_name")) %>% 
  rename(prey_spp=species) %>% 
  # Rearrange columns
  select(source, dietid, pred_comm, pred_spp, region, prey_comm, prey_spp, everything()) 

# Build stock info
stocks <- assessment %>% 
  # Reduce columns
  filter(mostrecent%in%c("999", "-1")) %>%
  select(assessid, assessorid, stockid, stocklong, assessyear, assessmethod) %>% 
  # Add reduced stock info
  left_join(select(stock, -c(tsn, inmyersdb, myersstockid, stocklong)), by="stockid") %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Rename columns
  rename(years=assessyear, method=assessmethod, species=scientificname, comm_name=commonname) %>% 
  # Correct important species names
  mutate(species=revalue(species, c("Loligo pealeii"="Doryteuthis pealeii",
                                    "Tetrapturus albidus"="Kajikia albida")))

# Completeness
# Should all be 0
complete(stocks)

# Build stock statistics
ts_stats <- timeseries_values_views %>% 
  # Sample size
  group_by(stockid) %>% 
  summarize(tl=sum(!is.na(TL)),
            tc=sum(!is.na(TC)),
            r=sum(!is.na(R)),
            tb=sum(!is.na(TB)),
            ssb=sum(!is.na(SSB))) %>% 
  mutate(catch=ifelse(tc>=tl, tc, tl),
         catch_type=ifelse(tc>=tl, "tc", "tl")) %>% 
  # Add units
  left_join(select(timeseries_units_views, stockid, TL, TC, R, TB, SSB), by="stockid") %>% 
  rename(tl_units=TL, tc_units=TC, r_units=R, tb_units=TB, ssb_units=SSB) %>% 
  mutate(catch_units=ifelse(tc>=tl, tc_units, tl_units)) 

# Add stock stats to stock info
stocks1 <- stocks %>%
  # Add TS stats
  left_join(ts_stats, by="stockid") %>% 
  # Filter stocks
  filter(tb_units=="MT" & catch_units=="MT") %>% 
  filter(tb>=20 & catch>=20) %>% 
  filter(!region %in% c("Australia", "Indian Ocean", "Mediterranean-Black Sea", 
                        "New Zealand", "Russia Japan", "South Africa", "West Africa",
                        "US Alaska", "Atlantic Ocean", "Pacific Ocean")) %>% 
  filter(country!="Argentina") %>% 
  # Add diet id
  mutate(region1=revalue(region, c("Canada East Coast"="NW Atlantic",
                                   "US East Coast"="NW Atlantic",
                                   "US Southeast and Gulf"="NW Atlantic",
                                   "Canada West Coast"="NE Pacific",
                                   "US West Coast"="NE Pacific",
                                   "Europe non EU"="Europe",
                                   "European Union"="Europe",
                                   "South America"="Humboldt")),
         dietid=paste(comm_name, region1)) %>% 
  # Remove stocks with diet info
  filter(!dietid %in% sort(unique(diets$dietid)))

# Make sure regions are removed
table(stocks1$region)
table(stocks1$region1)
table(stocks1$country)

