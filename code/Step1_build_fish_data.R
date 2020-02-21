

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
datadir1 <- "data/hilborn_etal_2017"
datadir2 <- "data/new_data"
outputdir <- "data"

# Read diet data
diet_orig <- read.csv(file.path(datadir, "predator_diet_proportions_final.csv"), as.is=T)
taxa <- read.csv(file.path(datadir, "taxanomic_info_final.csv"), as.is=T)

# Read RAM Legacy Database (v4.40)
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Assessment Data/DBdata (assessment data only).RData")


# Helper functions
################################################################################

# Function to calculate surplus production: 
# SP(t) = TB(t+1) - TB(t) + C(t)
# tb <- subset(bdata, assessid==unique(bdata$assessid)[1])$tb
# catch <- subset(bdata, assessid==unique(bdata$assessid)[1])$catch
calc_sp <- function(tb, catch){
  sp <- c(tb[2:length(tb)] - tb[1:(length(tb)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}

# Format data
################################################################################

# Modfiy taxa
taxa <- taxa %>% 
  mutate(species_sub=revalue(species_sub, c("Clupea pallasii pallasii"="Clupea pallasii")))

# Add scientific names to diet data
diet <- diet_orig %>%
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

# Important prey threshold
impt_prop <- 0.1

# Diet id key
diet_key <- diet %>%
  filter(prop_use_type!="none") %>% 
  group_by(dietid) %>% 
  summarise(nprey=n(),
            prey1=prey_comm[which.max(prop_use)], 
            prey1_prop=max(prop_use),
            nprey_impt=sum(prop_use>=impt_prop),
            prey_impt=paste(prey_comm[prop_use>=impt_prop], collapse=", "), 
            prey_impt_prop=sum(prop_use[prop_use>=impt_prop]),
            prey_prop=sum(prop_use))

# Identify useful stocks
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
  group_by(stockid) %>% 
  summarize(tl=sum(!is.na(TL)),
            tc=sum(!is.na(TC)),
            r=sum(!is.na(R)),
            tb=sum(!is.na(TB)),
            ssb=sum(!is.na(SSB)),
            bbmsy=sum(!is.na(BdivBmsypref))) %>% 
  mutate(catch=ifelse(tc>=tl, tc, tl),
         biomass=ifelse(tb>=ssb, tb, ssb),
         catch_type=ifelse(tc>=tl, "tc", "tl"),
         biomass_type=ifelse(tb>=ssb, "tb", "ssb")) %>% 
  left_join(select(timeseries_units_views, stockid, TL, TC, R, TB, SSB), by="stockid") %>% 
  rename(tl_units=TL, tc_units=TC, r_units=R, tb_units=TB, ssb_units=SSB) %>% 
  mutate(catch_units=ifelse(tc>=tl, tc_units, tl_units), 
         biomass_units=ifelse(tb>=ssb, tb_units, ssb_units))

# Export all stocks
write.csv(stocks, file.path(outputdir, "all_stocks.csv"), row.names=F)

# Prey data
################################################################################

# Filters
# 1. Prey species
# 2. Prey species in study region
# 3. Prey species in study region w/ biomass in MT
# 4. Prey species in study region w/ biomass in MT and >20 yr

# 1. Identify prey stocks
prey_spp <- sort(unique(diet$prey_spp)) 
stocks_prey1 <- stocks %>% 
  filter(species %in% prey_spp) %>% 
  select(-comm_name) %>% 
  left_join(select(taxa, species_sub, comm_name), by=c("species"="species_sub")) %>% 
  left_join(select(ts_stats, stockid, biomass, biomass_type, biomass_units, catch, catch_type, catch_units), by="stockid") %>% 
  select(assessid:species, comm_name, everything())
nrow(stocks_prey1)

# Any prey species in diet data without population data?
# Ammodytidae spp., Doryteuthis opalescens, Engraulis mordax
prey_spp[!prey_spp%in%sort(unique(stocks_prey1$species))] 

# 2. Identify prey stocks in study regions
table(stocks_prey1$region) # Eliminate West Africa, Russia/Japan, South Africa, Mediterranean-Black Sea
regions_type <- c("Canada East Coast", "Canada West Coast", 
                 "Europe non EU", "European Union", "US Alaska",
                 "US East Coast", "US Southeast and Gulf", "US West Coast",
                 "South America", "South Africa")
stocks_prey2 <- stocks_prey1 %>% 
  filter(region %in% regions_type)
nrow(stocks_prey2)

# 3. Identify prey stocks with >=15 year of biomass data
stocks_prey3 <- stocks_prey2 %>%
  filter(biomass>=15)
nrow(stocks_prey3)
table(stocks_prey3$biomass_type)

# Prey stocks to use
prey_stocks_use <- sort(stocks_prey3$stockid)

# Identify prey time series
# stockid, stocklong, year, tl, tl_units, tc, tc_units, catch, catch_type, catch_units,
# r, r_units, tb, tb_units, ssb, ssb_units, biomass, biomass_type, biomass_units
prey_ts <- timeseries_values_views %>% 
  # Reduce/rename columns
  select(stockid, stocklong, year, TL, TC, R, TB, SSB, BdivBmsypref) %>% 
  rename(tl=TL, tc=TC, r=R, tb=TB, ssb=SSB, bbmsy=BdivBmsypref) %>% 
  # Add units
  left_join(select(timeseries_units_views, stocklong, TL, TC, R, TB, SSB), by="stocklong") %>% 
  rename(tl_units=TL, tc_units=TC, r_units=R, tb_units=TB, ssb_units=SSB) %>% 
  # Add catch/biomass preffered
  left_join(select(ts_stats, stockid, biomass_type, catch_type), by="stockid") %>% 
  mutate(catch=ifelse(catch_type=="tc", tc, tl),
         catch_units=ifelse(catch_type=="tc", tc_units, tl_units),
         biomass=ifelse(biomass_type=="tb", tb, ssb),
         biomass_units=ifelse(biomass_type=="tb", tb_units, ssb_units)) %>% 
  # Reduce to stocks of interest
  filter(stockid %in% prey_stocks_use) %>% 
  # Build surplus production
  group_by(stockid) %>%
  mutate(sp=calc_sp(tb, catch),
         sp=ifelse(catch_units=="MT", sp, NA),
         sp_units=ifelse(catch_units=="MT", tb_units, NA)) %>%
  ungroup() %>% 
  # Rearrange columns
  select(stockid, stocklong, year, bbmsy, tl, tl_units, tc, tc_units, catch, catch_type, catch_units,
         r, r_units, tb, tb_units, ssb, ssb_units, biomass, biomass_type, biomass_units, sp, sp_units)

# Are the Hilborn et al. 2017 prey stocks included?
# (1) HERRNWATLC - Atlantic herring Northwestern Atlantic Coast...INCLUDED
# (2) Atlantic mackerel Gulf of Maine / Cape Hatteras.............use 2018 stock assessment
# (3) MENATLAN - Atlantic menhaden Atlantic.......................INCLUDED
# (4) California market squid.....................................not included, only landings data are available
# (5) MENATGM - Gulf menhaden Gulf of Mexico......................INCLUDED
# (6) LISQUIDATLC - Longfin inshore squid Atlantic Coast..........INCLUDED
# (7) Northern Anchovy............................................use MacCall et al. (2016) data (used by Hilborn et al)
# (8) CMACKPCOAST - Pacific chub mackerel Pacific Coast...........INCLUDED
# (9) PHAKEPCOAST - Pacific hake Pacific Coast....................INCLUDED
# (10) SARDPCOAST - Pacific sardine Pacific Coast.................INCLUDED
# (11) ILLEXNWATLC - Northern shortfin squid Atlantic Coast.......not included, uses abundance index
prey_stocks_use
filter(timeseries_units_views, stockid=="ILLEXNWATLC")

# Add new prey stocks
######################################

# PREY STOCKS TO ADD
# Northern anchovy
# Atlantic mackerel US East Coast
# Sandeel Area 4
# Northern shortfin squid Subareas 3+4
# Cury et al. (2011) prey indices

# Northern anchovy
hilborn <- read.csv(file.path(datadir1, "hilborn_etal_2017_prey_time_series.csv"), as.is=T)
nanch_orig <- filter(hilborn, stocklong=="Northern anchovy Pacific Coast")
nanch <- nanch_orig %>% 
  mutate(stockid="NANCHSCAL",
         ssb=ssb*1000,
         ssb_units="MT", 
         biomass=ssb,
         biomass_type="ssb",
         biomass_units="MT") %>% 
  select(stockid, stocklong, year, ssb, ssb_units, biomass, biomass_type, biomass_units)
  
# Atlantic mackerel
amack_orig <- read.csv(file.path(datadir2, "2018_atl_mackerel_assessment_data.csv"), as.is=T)
amack <- amack_orig %>% 
  select(year, catch_tot_mt, ssb_mt) %>% 
  rename(tc=catch_tot_mt, ssb=ssb_mt) %>% 
  mutate(stockid="ATLMACKUSEAST", 
         tc_units="MT",
         ssb_units="MT",
         biomass=ssb, biomass_type="ssb", biomass_units="MT",
         catch=tc, catch_type="tc", catch_units="MT")

# Sand eel Area 4
seel4_orig <- import(file.path(datadir2, "2016_SA4_sandeel_data.xlsx"))
seel4 <- seel4_orig %>% 
  select(Year, tb_mt, ssb_mt, catch_mt, r_e00) %>% 
  rename(year=Year, tc=catch_mt, tb=tb_mt, ssb=ssb_mt, r=r_e00) %>% 
  mutate(stockid="SANDEELSA4",
         tc_units="MT",
         ssb_units="MT",
         r_units="EOO",
         year=as.numeric(year), tc=as.numeric(tc), r=as.numeric(r),
         tb=as.numeric(tb), ssb=as.numeric(ssb),
         biomass=tb, biomass_type="tb", biomass_units="MT",
         catch=tc, catch_type="tc", catch_units="MT",
         sp=calc_sp(tb, catch), sp_units="MT")

# Sand eel Area 7
seel7_orig <- read.csv(file.path(datadir2, "2007_SA7_sandeel_data.csv"))
seel7 <- seel7_orig %>% 
  rename(tb=biomass_mt) %>% 
  mutate(stockid="SANDEELSA7",
         tb_units="MT",
         biomass=tb, biomass_type="tb", biomass_units="MT")

# Northern shortfin squid
illex_orig <- import(file.path(datadir2, "2016_NAFO_shortfin_squid_data.xlsx"))
illex <- illex_orig %>% 
  select(-f) %>% 
  rename(tc=catch_mt, tb=index_kgtow) %>% 
  mutate(stockid="ILLEXNAFO3-4",
         tc_units="MT",
         tb_units="relative",
         year=as.numeric(year), tc=as.numeric(tc), tb=as.numeric(tb),
         biomass=tb, biomass_type="tb", biomass_units="relative",
         catch=tc, catch_type="tc", catch_units="MT")

# Read Cury et al. (2011) data
cury_orig <- read.csv("data/cury_etal_2011/cury_etal_2011_prey_populations.csv", as.is=T)
cury_ts_orig <- read.csv("data/cury_etal_2011/cury_etal_2011_prey_time_series.csv", as.is=T)

# Build Cury et al. stock key to add to "non-RAM" key read in below
cury <- cury_orig %>% 
  filter(nyrs>=15) %>% 
  mutate(stockid=revalue(stocklong, c("Australian krill Kaikoura"="AUSKRILLKAI",
                                      "Atlantic herring Rost age 0"="ATLHERRROST",
                                      "Pacific sardine Robben Island spawners"="PSARDROBBEN_SPWN",
                                      "Pacific sardine Robben Island YOY"="PSARDROBBEN_YOY",
                                      "European anchovy Robben Island spawners"="EANCHOROBBEN_SPWN",
                                      "European anchovy Robben Island YOY"="EANCHOROBBEN_YOY",
                                      "Rockfish spp. SE Farallon Island MRI1"="ROCKSEFI1",
                                      "Rockfish spp. SE Farallon Island MRI2"="ROCKSEFI2",
                                      "Rockfish spp. SE Farallon Island MRI3"="ROCKSEFI3"))) %>% 
  rename(assessorid=ref, species=sci_name, areaname=location, biomass_units=n_units) %>% 
  select(assessorid, stockid, stocklong, species, comm_name, areaname, region, biomass_units)
         
# Build Cury et al. time series
cury_ts <- cury_ts_orig %>% 
  filter(stocklong %in% cury$stocklong) %>% 
  left_join(select(cury, stocklong, stockid), by="stocklong") %>% 
  rename(biomass=n, biomass_units=n_units) %>% 
  mutate(biomass_type=revalue(biomass_units, c("g/m3"="relative",
                                               "index"="relative",
                                               "million tons"="ssb"))) %>%
  select(stockid, stocklong, year, biomass, biomass_type, biomass_units)

# Merge non-RAM stocks
prey_ts_not_ram <- rbind.fill(nanch, amack, seel4, seel7, illex, cury_ts)

# Merge RAM stocks with non-RAM stocks
prey_ts_final <- rbind.fill(prey_ts, prey_ts_not_ram)

# Read non-RAM stock key
non_ram_key <- import(file.path(datadir2, "stocks_not_in_ram.csv"), na.strings="")

# Add Cury stocks to non-RAM key
non_ram_key <- rbind.fill(non_ram_key, cury)

# Build non-RAM stats
non_ram_stats <- prey_ts_not_ram %>% 
  group_by(stockid) %>% 
  summarize(years=paste(range(year), collapse="-"),
            catch=sum(!is.na(catch)),
            biomass=sum(!is.na(biomass)))

# Add stats to key
non_ram_key <- left_join(non_ram_key, non_ram_stats, by="stockid")

# Add non-RAM stocks to stock key
stocks_prey4 <- rbind.fill(stocks_prey3, non_ram_key)


# Predator data
################################################################################

# Filters
# 1. Predator species
# 2. Predator species in study region
# 3. Predator species in study region w/ TB/catch in MT
# 4. Predator species in study region w/ TB/catch in MT and >20 yr
# 5. Predator species in study region w/ TB/catch in MT and >20 yr and diet info
# 6. Predator species in study region w/ TB/catch in MT and >20 yr and diet info + 20% thresh

# 1. Identify predator stocks
pred_spp <- sort(unique(diet$pred_spp)) 
stocks_pred1 <- stocks %>% 
  # Reduce to predator species
  filter(species %in% pred_spp) %>% 
  left_join(taxa, by=c("species"="species_sub")) %>% 
  # Add time series info
  left_join(select(ts_stats, stockid, 
                   catch, catch_type, catch_units, 
                   tb, tb_units), by="stockid") %>% 
  # Rearrange and rename columns
  select(assessid:method, areaid, areaname, region, class:comm_name.y, catch:tb_units) %>% 
  rename(species=species.y, comm_name=comm_name.y)
nrow(stocks_pred1)

# 2. Identify pred stocks in study regions
table(stocks_pred1$region) # Eliminate West Africa, Russia/Japan, South Africa, Atlantic Ocean, Indian Ocean, Pacific Ocean, Mediterranean-Black Sea
regions_type <- c("Canada East Coast", "Canada West Coast", 
                 "Europe non EU", "European Union",
                 "US Alaska", "US East Coast", "US Southeast and Gulf", "US West Coast",
                 "South America",
                 "Atlantic Ocean", "Pacific Ocean")
stocks_pred2 <- stocks_pred1 %>% 
  filter(region %in% regions_type)
nrow(stocks_pred2)

# 3. Identify pred stocks with TB/catch in metric tons
stocks_pred3 <- stocks_pred2 %>%
  filter(tb_units=="MT" & catch_units=="MT")
nrow(stocks_pred3)

# 4. Identify pred stocks with TB/catch in metric tons
stocks_pred4 <- stocks_pred3 %>%
  filter(tb>=20 & catch>=20)
nrow(stocks_pred4)

# 5. Identify pred stock with diet info
sort(unique(stocks_pred4$region))
stocks_pred4_hms <- subset(stocks_pred4, region%in%c("Atlantic Ocean", "Pacific Ocean"), select=c(stockid, region, areaname))
sort(unique(stocks_pred4_hms$areaname))
us_east_areas <- c("Atlantic Ocean", "Northern Atlantic") # Not South Atlantic
us_west_areas <- c("Eastern Pacific", "North Pacific Ocean", 
                   "Northeast Pacific", "Pacific Ocean") # Not Western Pacific Ocean, Central Western Pacific Ocean, Western and Central North Pacific
stocks_pred5 <- stocks_pred4 %>% 
  # Add diet id
  mutate(region1=revalue(region, c("Canada East Coast"="NW Atlantic",
                                   "Canada West Coast"="NE Pacific",
                                   "Europe non EU"="Europe",
                                   "European Union"="Europe",
                                   "South America"="Humboldt",
                                   "US Alaska"="NE Pacific",
                                   "US East Coast"="NW Atlantic",
                                   "US Southeast and Gulf"="NW Atlantic",
                                   "US West Coast"="NE Pacific")),
         region1=ifelse(region1=="Atlantic Ocean" & areaname%in%us_east_areas, "NW Atlantic", region1),
         region1=ifelse(region1=="Pacific Ocean" & areaname%in%us_west_areas, "NE Pacific", region1),
         dietid=paste(comm_name, region1)) %>% 
  # Add prey info
  left_join(diet_key, by="dietid") %>% 
  # Filter those without diet info
  filter(nprey>0)
nrow(stocks_pred5)

# 5. Identify pred stocks w/ >20% diet prey
stocks_pred6 <- stocks_pred5 %>% 
  filter(prey_prop>=0.20)
nrow(stocks_pred6)

# Predator stocks to use
pred_stocks_use <- sort(stocks_pred6$stockid)

# Identify predator time series
# stockid, stocklong, year, tl, tl_units, tc, tc_units, catch, catch_type, catch_units,
# r, r_units, tb, tb_units, ssb, ssb_units, biomass, biomass_type, biomass_units
pred_ts <- timeseries_values_views %>% 
  # Reduce/rename columns
  select(stockid, stocklong, year, TL, TC, R, TB, SSB) %>% 
  rename(tl=TL, tc=TC, r=R, tb=TB, ssb=SSB) %>% 
  # Add units
  left_join(select(timeseries_units_views, stocklong, TL, TC, R, TB, SSB), by="stocklong") %>% 
  rename(tl_units=TL, tc_units=TC, r_units=R, tb_units=TB, ssb_units=SSB) %>% 
  # Add catch/biomass preffered
  left_join(select(ts_stats, stockid, biomass_type, catch_type), by="stockid") %>% 
  mutate(catch=ifelse(catch_type=="tc", tc, tl),
         catch_units=ifelse(catch_type=="tc", tc_units, tl_units),
         biomass=ifelse(biomass_type=="tb", tb, ssb),
         biomass_units=ifelse(biomass_type=="tb", tb_units, ssb_units)) %>% 
  # Reduce to stocks of interest
  filter(stockid %in% pred_stocks_use) %>% 
  # Build surplus production
  group_by(stockid) %>%
  mutate(sp=calc_sp(tb, catch),
         sp_units=tb_units) %>%
  ungroup() %>% 
  # Rearrange columns
  select(stockid, stocklong, year, tl, tl_units, tc, tc_units, catch, catch_type, catch_units,
         r, r_units, tb, tb_units, ssb, ssb_units, biomass, biomass_type, biomass_units, sp, sp_units)


# Are the Hilborn et al. 2017 fish predator stocks included?
################################################################################

# Stockids used
pred_stocks_use

# 1. Check Hilborn predators stocks with a stockid
# List whether it is INCLUDED or the reason it is not included

# ALBANATL Albacore tuna Northern Atlantic.......................INCLUDED (though HMS)
# ALBANPAC Albacore tuna North Pacific Ocean.....................INCLUDED (though HMS)
# ARFLOUNDPCOAST Arrowtooth flounder Pacific Coast...............INCLUDED
# ATBTUNAWATL Atlantic bluefin tuna Western Atlantic.............no TB data (also HMS)
# ATHAL5YZ Atlantic halibut Gulf of Maine / Georges Bank.........INCLUDED
# ATLCROAKMATLC Atlantic croaker Mid-Atlantic Coast..............no diet info
# BIGEYEATL Bigeye tuna Atlantic Ocean...........................INCLUDED (though HMS)
# BLACKROCKNPCOAST Black rockfish Northern Pacific Coast.........INCLUDED, kinda - the old assessment had 2 stocks (N/S) and the new assessment has 3 stocks (CA/OR/WA)
# BLUEFISHATLC Bluefish Atlantic Coast...........................INCLUDED
# BMARLINPAC Blue marlin Pacific Ocean...........................no diet info
# BSBASSMATLC Black sea bass Mid-Atlantic Coast..................INCLUDED
# BTIPSHARATL Blacktip shark Atlantic............................no TB data, TC data in E00
# BTIPSHARGM Blacktip shark Gulf of Mexico.......................no TB data, TC data in E00
# CODGB Atlantic cod Georges Bank................................no TB data
# FTOOTHSHARATL Finetooth shark Atlantic.........................no TB data, TC data in E00
# LINGCODNPCOAST Lingcod Northern Pacific Coast..................INCLUDED
# POLL5YZ Pollock Gulf of Maine / Georges Bank...................INCLUDED
# SABLEFPCOAST Sablefish Pacific Coast...........................INCLUDED
# SAURNWPAC Pacific saury Northwest Pacific......................only 11 yr of TB data
# SBARSHARATL Sandbar shark Atlantic.............................no TB data, TC data in E00
# SCUPNWATLC Scup Northwestern Atlantic Coast....................no TB data
# SDOGATLC Spiny dogfish Atlantic Coast..........................only 16 yr of TB data
# SFLOUNMATLC Summer flounder Mid-Atlantic Coast.................no TB data
# SHAKEGOMNGB Silver hake Gulf of Maine / Georges Bank...........no TB data
# SHAKESGBMATL Silver hake Southern Georges Bank / Mid-Atlantic..no TB data
# SPSDOGPCOAST Spotted spiny dogfish Pacific Coast...............INCLUDED
# SSTHORNHPCOAST Shortspine thornyhead Pacific Coast.............INCLUDED
# STRIPEDBASSGOMCHATT Striped bass Gulf of Maine / Cape Hatteras.no TB data, TC data in E00
# SWORDNATL Swordfish Northern Atlantic..........................INCLUDED (though HMS)
# WEAKFISHATLC Weakfish Atlantic Coast...........................no TB data
# WHAKEGBGOM White hake Gulf of Maine / Georges Bank.............INCLUDED
# WMARLINATL White marlin Atlantic Ocean.........................INCLUDED (though HMS)
# WROCKPCOAST Widow rockfish Pacific Coast.......................INCLUDED
# YFINATL Yellowfin tuna Atlantic Ocean..........................INCLUDED (though HMS)
# YTROCKNPCOAST Yellowtail rockfish Northern Pacific Coast.......INCLUDED

# 2. Check Hilborn predators stocks without a stockid
# Identify (1) ones already included or that you should include or (2) ones not to include and why

# ALREADY INCLUDED:
# Smooth dogfish Northwest Atlantic Coast........................SMDOGATL, already included

# TO EXCLUDE
# Red drum Florida-South Carolina................................In SEDAR 44 (2015) but SSB not TB
# Red drum North of North Carolina...............................In SEDAR 44 (2015) but SSB not TB
# Offshore hake Northwest Atlantic Coast.........................In NEFSC 2010 but too complicated (fall/winter/spring swept area biomass)
# Atlantic sharpnose shark Northwest Atlantic & GOM..............SNOSESHARATL but TC in E00 and no TB data
# Pacific bluefin tuna Pacific Ocean.............................PACBTUNA but Pacific region
# Pollock NAFO-4VWX5.............................................POLL4VWX but no TB data
# Striped marlin North Pacific...................................STMARLINWCNPAC/STMARLINNEPAC but Pacific region

# TO EXCLUDE:
# Blue shark Chesapeake Bay......................................CPUE not biomass (Simpfendorfer et al. 2002)
# Blue shark Chesapeake Bay......................................CPUE not biomass (Simpfendorfer et al. 2002)
# Common and bigeye thresher sharks Northwest Atlantic...........CPUE not biomass (Cortes et al. 2007)
# Common thresher shark West Coast US............................CPUE not biomass (NMFS unpublished)
# Cutthroat trout Nestucca River.................................not a marine species
# Cutthroat trout Wilson and Tasks Rivers........................not a marine species
# Dolphinfish Gulf of Mexico.....................................CPUE not biomass (Kleisner 2008)
# Dolphinfish Western Atlantic...................................CPUE not biomass (Kleisner 2008)
# Dusky shark Northwest Atlantic & GOM...........................CPUE not biomass (Morgan 2008) - DSKSHARGMATL but no TC/TB data
# Eastern Pacific bonito Southern California.....................CPUE not biomass (Mandy 2008)
# Little skate Georges Bank......................................CPUE not biomass (Nye et al. 2010) - LSKAT5YCHATT but no diet data
# Little skate Gulf of Maine.....................................CPUE not biomass (Nye et al. 2010) - LSKAT5YCHATT but no diet data
# Night shark Northwest Atlantic.................................CPUE not biomass (Cortes et al. 2007)
# Northern shortfin squid North Carolina-Scotian Shelf...........CPUE not biomass (NEFSC 2003) - ILLEXNWATLC but TB is relative
# Northern shortfin squid Scotian Shelf, Canada..................CPUE not biomass (NEFSC 2003) - ILLEXNWATLC but TB is relative
# Red hake Gulf of Maine.........................................CPUE not biomass (Nye et al. 2010) - RHAKEGOMNGB but no TB data
# Shortfin mako shark Northwest Atlantic.........................CPUE not biomass (Cortes et al. 2007)
# Shortfin mako shark Scotian Shelf, Canada......................CPUE not biomass (Campana et al. 2005)
# Silky shark Northwest Atlantic.................................CPUE not biomass (Cortes et al. 2007)
# Spinner shark Florida-Maryland & Gulf Coast-Louisiana..........CPUE not biomass (Carlson et al. 2012)


# Export data
################################################################################

# Rename
prey_ts <- prey_ts_final
prey_stocks <- stocks_prey4
pred_stocks <- stocks_pred6

# Export data
save(prey_ts, prey_stocks,
     pred_ts, pred_stocks,
     file=file.path(outputdir, "potential_fish_predator_prey_data.Rdata"))
            



