
# Build the following files from the original Hilborn et al. 2017 data:

# 1. Taxanomic info
# type, class, order, family, genus, species, sub-species, comm_name

# 2. Stock info
# Prey: stocklong, stocklong_orig, stockid, ocean, region, area, species, comm_name, nyr, n_units, reference
# Predators: stocklong, stocklong_orig, stockid, ocean, region, area, species, comm_name, nyr, n_units, reference

# 3. Time series
# Prey: stocklong, stocklong_orig, year, ssb, r, tb, n, n_units 
# Predators: stocklong, stocklong_orig, reference, year, year_orig, n, n_units, catch

# 4. Diet data
# pred_type, pred_comm_name, pred_species,
# ocean, region, area, reference, prey_species, prey_comm_name, n_data,
# years, season, year1, month1, month_name1, year2, month2, month_name2,
# method_length, method_diet, comment, n_samples, pred_stage1, pred_stage2,
# prey_mm_min, prey_mm_max, prey_mm_range, prey_mm_med, prey_mm_avg, prey_mm_sd,
# prop_diet_by_wt, prop_diet_by_wt_type, prop_diet_by_n, prop_diet_by_energy,
# prop_diet_by_occur, prop_diet_by_occur_sd, prop_diet_by_index

# 5. Prey importance
# This appears to be a product generated from the diet data file. I clean it up
# but ultimately use my own processing of the diet data.

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(Hmisc) # capitalize()
library(stringr) # word()
library(rfishbase)

# Directories
inputdir <- "data/hilborn_etal_2017/orig"
outputdir <- "data/hilborn_etal_2017/"
ramdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ramldb/ramldb_v3.8"

# Read RAM stocks
ram <- read.csv(paste(ramdir, "all_stocks_in_ramldb.csv", sep="/"), as.is=T)

# Read taxanomic info
taxa_orig <- read.csv(paste(inputdir, "sciname_key.csv", sep="/"), as.is=T)

# Read abundance time series
pred_ts_orig <- read.csv(paste(inputdir, "original Nicole_PredatorAbundance.csv", sep="/"), as.is=T)
prey_ts_orig <- read.csv(paste(inputdir, "original Nicole_preyAbundance.csv", sep="/"), as.is=T)
diets_orig <- read.csv(paste(inputdir, "original DietData.csv", sep="/"), as.is=T)
prey_impt_orig <- read.csv(paste(inputdir, "original PreyImportance.csv", sep="/"), as.is=T)


# Build FB/SLB taxa key
################################################################################

# FishBase/SeaLifeBase taxa keys
taxa_key_fb <- load_taxa()
taxa_key_slb <- sealifebase

# Add source column
taxa_key <- taxa_key_fb %>% 
    bind_rows(taxa_key_slb) %>% 
    setNames(tolower(names(.))) %>% 
    mutate(sciname=paste(genus, species)) %>% 
    select(-species) %>% 
    rename(species=sciname) %>% 
    select(class, order, family, genus, species) %>% 
    unique()

# Check for duplicated scientific names
sum(duplicated(taxa_key$species))


# Format taxanomic info
################################################################################

# Expand taxanomic info
taxa <- taxa_orig %>%
  mutate(species_sub=species,
         species=word(species_sub, 1, 2)) %>% 
  left_join(taxa_key, by="species") %>% 
  select(class, order, family, genus, species, species_sub, comm_name) %>% 
  arrange(species)

# Check for duplicated species
taxa$species_sub[duplicated(taxa$comm_name)]
taxa$species_sub[duplicated(taxa$species_sub)]

# Fix Alopias spp., Globicephala spp., and Clupea pallasii
taxa[taxa$species=="Alopias spp.",c("class", "order", "family", "genus")] <- taxa[taxa$species=="Alopias vulpinus",c("class", "order", "family", "genus")]
taxa[taxa$species=="Globicephala spp.",c("class", "order", "family", "genus")] <- taxa[taxa$species=="Globicephala melas",c("class", "order", "family", "genus")]
taxa[taxa$species=="Clupea pallasii",c("class", "order", "family", "genus")] <- taxa[taxa$species=="Clupea harengus",c("class", "order", "family", "genus")]

# Fix Ardenna species (shearwaters)
ardenna_spp <- c("Ardenna grisea", "Ardenna tenuirostris")
taxa$class[taxa$species%in%ardenna_spp] <- "Aves"
taxa$order[taxa$species%in%ardenna_spp] <- "Procellariiformes"
taxa$family[taxa$species%in%ardenna_spp] <- "Procellariidae"
taxa$genus[taxa$species%in%ardenna_spp] <- "Ardenna"

# Fix osprey (Pandion haliaetus)
taxa$class[taxa$species=="Pandion haliaetus"] <- "Aves"
taxa$order[taxa$species=="Pandion haliaetus"] <- "Accipitriformes"
taxa$family[taxa$species=="Pandion haliaetus"] <- "Pandionidae"
taxa$genus[taxa$species=="Pandion haliaetus"] <- "Pandion"

# Add "type" column
taxa$type <- ifelse(taxa$class=="Aves", "birds", ifelse(taxa$class=="Mammalia", "mammals", "fish"))

# Rearrange/order columns
taxa <- select(taxa, type, everything())
taxa <- arrange(taxa, type, class, order, family, species)

# Check completeness
apply(taxa, 2, function(x) sum(is.na(x)))


# Format prey data
################################################################################

# Files to build:
# Prey stock info: stocklong, stocklong_orig, stockid, region, area, species, comm_name, nyr, reference
# Prey time series: stocklong, stocklong_orig, year, ssb, r, tb, n, n_units 

# Lingering questions:
# Is "Shortfin squid Atlantic Coast" the RAMLDB "Northern shortfin squid Northwestern Atlantic Coast" stock (ILLEXNWATLC)?

# Format prey time series
colnames(prey_ts_orig) <- tolower(colnames(prey_ts_orig))
prey_ts <- prey_ts_orig %>%
  rename(n=abundance, n_units=abundancetype)

# Build prey stock info
prey_stocks <- prey_ts %>%
  group_by(stocklong, n_units) %>% 
  summarize(nyr=sum(!is.na(n))) %>% 
  left_join(select(ram, stocklong, stockid, region, area, comm_name), by="stocklong") %>% 
  group_by() %>% 
  mutate(ocean=NA,
         reference=ifelse(!is.na(stockid), "RAM?", "Other?"),
         comm_name=revalue(comm_name, c("Herring"="Atlantic herring",
                                        "Chub mackerel"="Pacific chub mackerel",
                                        "Sardine"="Pacific sardine"))) %>% 
  select(stocklong, stockid, ocean, region, area, comm_name, nyr, n_units, reference)

# Fill in regions
west.coast <- c("California Market Squid", "Northern Anchovy")
east.coast <- c("Shortfin squid Atlantic Coast", "Atlantic mackerel Gulf of Maine / Cape Hatteras")
prey_stocks$region[prey_stocks$stocklong%in%west.coast] <- "US West Coast"
prey_stocks$region[prey_stocks$stocklong%in%east.coast] <- "US East Coast"

# Fill in California Market Squid
prey_stocks$area[prey_stocks$stocklong=="California Market Squid"] <- "Pacific Coast"
prey_stocks$comm_name[prey_stocks$stocklong=="California Market Squid"] <- "California market squid"

# Fill in Northern anchovy
prey_stocks$area[prey_stocks$stocklong=="Northern Anchovy"] <- "Pacific Coast"
prey_stocks$comm_name[prey_stocks$stocklong=="Northern Anchovy"] <- "Northern anchovy"

# Fill in Shortfin squid Atlantic Coast
prey_stocks$area[prey_stocks$stocklong=="Shortfin squid Atlantic Coast"] <- "Atlantic Coast"
prey_stocks$comm_name[prey_stocks$stocklong=="Shortfin squid Atlantic Coast"] <- "Northern shortfin squid"

# Fill in Atlantic mackerel Gulf of Maine / Cape Hatteras
prey_stocks$area[prey_stocks$stocklong=="Atlantic mackerel Gulf of Maine / Cape Hatteras"] <- "Gulf of Maine / Cape Hatteras"
prey_stocks$comm_name[prey_stocks$stocklong=="Atlantic mackerel Gulf of Maine / Cape Hatteras"] <- "Atlantic mackerel"

# Add species and stocklong corrected
prey_stocks <- prey_stocks %>% 
  left_join(select(taxa, comm_name, species_sub), by="comm_name") %>% 
  rename(species=species_sub) %>% 
  mutate(stocklong_orig=stocklong,
         stocklong=paste(comm_name, area), 
         ocean=ifelse(region%in%c("US East Coast", "US Southeast and Gulf"), "NW Atlantic", "NE Pacific")) %>% 
  select(stocklong, stocklong_orig, stockid, ocean, region, area, species, comm_name, nyr, n_units, reference)

# Make sure stocklong is a unique identifier and check completeness
sum(duplicated(prey_stocks$stocklong))
apply(prey_stocks, 2, function(x) sum(is.na(x)))


# Format prey time series
#########################################

# Add new stockid to time series data
prey_ts <- prey_ts %>% 
  rename(stocklong_orig=stocklong) %>% 
  left_join(select(prey_stocks, stocklong, stocklong_orig), by="stocklong_orig") %>% 
  select(stocklong, stocklong_orig, everything()) %>% 
  mutate(n_units=ifelse(!is.na(n), n_units, NA))

# Check completeness
apply(prey_ts, 2, function(x) sum(is.na(x)))


# Format predator data
################################################################################

# Files to build:
# Pred stock info: stocklong, stocklong_orig, stockid, region, area, species, comm_name, nyr, reference
# Pred time series: stocklong, stocklong_orig, year, n, n_units, catch

# Lingering questions:
# Pollock NAFO-4VWX5 and Striped marlin North Pacific are from RAM but don't match any stocklongs
# Pollock NAFO-4VWX5 could be POLL4VWX Pollock Scotian Shelf and Bay of Fundy
# Striped marlin North Pacific could be STMARLINNEPAC Striped marlin Northeast Pacific

# Notes:
# The column "unique" represents the unique identifier for each predator stock
# and appears to have been formed by concatenating species, area, and abundance type.
# Sometimes, data for the unique stocks come from multiple sources, therefore the reference
# does not contribute to the unique identifier.

# Format predator time series
colnames(pred_ts_orig) <- tolower(colnames(pred_ts_orig))
pred_ts <- pred_ts_orig %>%
  rename(stock=specie, n=abundance, n_units=abundance_type, region=large.area, stocklong=unique) %>% 
  select(stocklong, stock, region, area, reference, year, n, n_units, catch) %>% 
  mutate(area=trimws(area),
         region=trimws(region),
         stock=trimws(stock),
         n_units=tolower(trimws(n_units)),
         n_units=revalue(n_units, c("abundance estimate"="abundance (est.)",
                                    "abundance index_meankg_pertow"="abundance index (mean kg/tow)", 
                                    "abundanceages1-3"="abundance (ages 1-3)",
                                    "cpue_tons_trip"="cpue (tons/trip)",
                                    "density autumn"="density (fall)",
                                    "density spring"="density (spring)",
                                    "density summer"="density (summer)",
                                    "density winter"="density (winter)",
                                    "logbiomass"="biomass (log-scale)",
                                    "minimum abundance estimate"="abundance (min est.)",
                                    "estimated abundance"="abundance (est.)",
                                    "index of abundance"="abundance index",
                                    "geometric mean abundance"="abundance (geometric mean)",
                                    "predicted abundance"="abundance (pred.)",
                                    "predicted biomass"="biomass (pred.)",
                                    "recruitment index_nless30cm_per_tow"="recruitment index (# less than 30 cm / tow)",
                                    "standardizedcpue"="cpue (standardized)",
                                    "standardizedcpue_bllop"="cpue (standardized, bllop)",
                                    "standardizedcpue_females"="cpue (standardized, females)",
                                    "standardizedcpue_lps"="cpue (standardized, lps)",              
                                    "standardizedcpue_males"="cpue (standardized, males)",  
                                    "standardizedcpue_pllop"= "cpue (standardized, pllop)",                    
                                    "standardizedcpue_vims"="cpue (standardized, vims)",  
                                    "standarizedcpue"="cpue (standardized)",
                                    "total biomass"="biomass (total)",
                                    "total counts"="abundance (total)",
                                    "total estimated population"="abundance (total est.)",
                                    "total nests"="nests (total)",
                                    "weigthed average abundance"="abundance (weighted mean)")),
         reference=gsub("etal", " et al. ", reference), 
         reference=gsub(" et al ", " et al. ", reference),
         reference=gsub("NOAAStocAssessment", "NOAA ", reference),
         reference=gsub("NOAAStockAssessment", "NOAA ", reference),
         reference=revalue(reference, c("Hemmil2005"="Hemmil 2005",
                                        "http://www.esasuccess.org/birds.shtml"="esasuccess.org",
                                        "Kleisner2008"="Kleisner 2008",
                                        "Mandy2008"="Mandy 2008",
                                        "Morgan2008"="Morgan 2008",
                                        "WattsandPaxton2007"="Watts and Paxton 2007",
                                        "Offshore Hake Assessment2010"="Offshore Hake Assessment 2010",
                                        "ORNativeFishStatusReport_Vol_II"="OR Native Fish Status Report Vol II",
                                        "RedDrumStockAssmtSummary2009"="Red Drum Stock Assessment Summary 2009",
                                        "SEDAR_StockAssessment2013-Atlantic Sharpnose shark"="SEDAR 2013",
                                        "SEDAR_StockAssessment2015-Atlantic Smooth dogfish shark"="SEDAR 2015",
                                        "Shortfin squid_NEFSC_NOOAReport2003"="NEFSC 2003 Shortfin Squid Report",
                                        "StockAssessment_of_PaicifcBluefinTuna2014"="PBT Stock Assessment 2014")))
pred_ts[pred_ts==""] <- NA

# Inspect
sort(unique(pred_ts$n_units))
sort(unique(pred_ts$reference))


# Build predator stock info
#########################################

# Build predator stock info
pred_stocks <- pred_ts %>%
  group_by(stocklong, stock, region, area, n_units) %>%
  summarize(nyr=sum(!is.na(n)),
            reference=paste(sort(unique(reference)), collapse=", ")) %>%
  arrange(desc(nyr))

# Format RAM predator stocks
#########################################

# Format predators in RAM database
pred_stocks_ram <- pred_stocks %>% 
  group_by() %>% 
  filter(reference=="RAM") %>% 
  select(stocklong, stock, reference, nyr, n_units) %>% 
  rename(stocklong_orig=stocklong, stocklong=stock) %>% 
  left_join(select(ram, stocklong, stockid, region, area, comm_name), by="stocklong") %>% 
  mutate(comm_name=revalue(comm_name, c("Atlantic Halibut"="Atlantic halibut",
                                        "White Marlin"="White marlin",
                                        "Silver Hake"="Silver hake"))) %>%
  left_join(select(taxa, comm_name, species_sub), by="comm_name") %>% 
  rename(species=species_sub) %>% 
  select(stocklong, stocklong_orig, stockid, region, area, species, comm_name, nyr, n_units, reference)

# Fill gaps
pred_stocks_ram$region[pred_stocks_ram$stocklong=="Pollock NAFO-4VWX5"] <- "Canada East Coast"
pred_stocks_ram$area[pred_stocks_ram$stocklong=="Pollock NAFO-4VWX5"] <- "NAFO-4VWX5"
pred_stocks_ram$species[pred_stocks_ram$stocklong=="Pollock NAFO-4VWX5"] <- "Pollachius virens"
pred_stocks_ram$comm_name[pred_stocks_ram$stocklong=="Pollock NAFO-4VWX5"] <- "Pollock"
pred_stocks_ram$region[pred_stocks_ram$stocklong=="Striped marlin North Pacific"] <- "Pacific Ocean"
pred_stocks_ram$area[pred_stocks_ram$stocklong=="Striped marlin North Pacific"] <- "North Pacific"
pred_stocks_ram$species[pred_stocks_ram$stocklong=="Striped marlin North Pacific"] <- "Kajikia audax"
pred_stocks_ram$comm_name[pred_stocks_ram$stocklong=="Striped marlin North Pacific"] <- "Striped marlin"

# Final formatting
pred_stocks_ram <- pred_stocks_ram %>% 
  mutate(stocklong=paste(comm_name, area)) %>% 
  select(stocklong, stocklong_orig, stockid, region, area, species, comm_name, nyr, n_units, reference)

# Make sure stocklong is a unique identifier and check completeness
sum(duplicated(pred_stocks_ram$stocklong))
apply(pred_stocks_ram, 2, function(x) sum(is.na(x)))


# Format other predator stocks
#########################################

# Format other predator stocks
pred_stocks_other <- pred_stocks %>% 
  group_by() %>% 
  filter(reference!="RAM") %>% 
  rename(comm_name=stock) %>% 
  mutate(comm_name=capitalize(tolower(trimws(comm_name))),
         comm_name=revalue(comm_name, c("Brandt cormoran"="Brandt cormorant",
                                        "California gulls"="California gull",
                                        "Common and bigeye thresher shark"="Common and bigeye thresher sharks",
                                        "Common murre"="Common guillemot",
                                        "Dalls porpoise"="Dall's porpoise",
                                        "Pacific bonito"="Eastern Pacific bonito",
                                        "Bluefin tuna"="Pacific bluefin tuna", # Pacific stock
                                        "Gray seal"="Grey seal",
                                        "Hampback whale"="Humpback whale",
                                        "Pacific harbour seal"="Pacific harbor seal",
                                        "Shortfin squid"="Northern shortfin squid",
                                        "Thresher shark"="Common thresher shark",
                                        "Western atlantic harbor seal"="Western Atlantic harbor seal",
                                        "Xantus murrel"="Xantus's murrellet"))) %>% 
  left_join(select(taxa, comm_name, species_sub), by="comm_name") %>% 
  rename(species=species_sub) %>% 
  mutate(region=trimws(region),
         region=revalue(region, c("AK"="Alaska",
                                  "Atlantic coast"="Atlantic Coast",
                                  "CA"="California",
                                  "WA"="Washington",
                                  "OR"="Oregon",
                                  "CA and OR"="CA/OR",
                                  "CA_OR_WA"="CA/OR/WA",
                                  "West Coast"="US West Coast",
                                  "Chesapeak Bay"="Chesapeake Bay",
                                  "North West Atlantic"="NW Atlantic",
                                  "Northwest Atlantic Coast and GOM"="NW Atlantic Coast & Gulf of Maine",
                                  "North West Atlantic_and_GOM"="NW Atlantic Coast & Gulf of Maine" )),
         area=trimws(area),
         area=revalue(area, c("CA"="California",
                              "CA_OR_WA"="CA/OR/WA",
                              "California and Oregon"="CA/OR",
                              "Central Virginia to lower Bay of Fundy"="Central Virginia-Lower Bay of Fundy",
                              "Eastern pacific"="Eastern Pacific",
                              "FromFlorida_TO_Maryland_andGulfCoast_to_Alabama"="Florida-Maryland & Gulf Coast-Alabama",
                              "FromFlorida_TO_Maryland_andGulfCoast_to_Lousiana"="Florida-Maryland & Gulf Coast-Louisiana",
                              "FromFlorida_to_SouthCarolina"="Florida-South Carolina",
                              "George Bank"="Georges Bank",
                              "Georges Bank to mouth of Gulf of St Lawrence"="Georges Bank-Gulf of St. Lawrence",
                              "Gulf Farallonls"="Gulf of Farallon Islands",
                              "Gulf of Maine_bay of Fundy"="Gulf of Maine-Bay of Fundy",
                              "MonterreyBay"="Monterrey Bay",
                              "Non-Sable Is"="Non-Sable Islands",
                              "North Carolina to Scotian Shelf"="North Carolina-Scotian Shelf",
                              "North of NorthCarolina"="North of North Carolina",
                              "North West Atlantic"="Northwest Atlantic",
                              "North West Atlantic_and_GOM"="Northwest Atlantic & GOM",
                              "Northwest Atlantic Coast"="Northwest Atlantic Coast",
                              "Northwest Atlantic Coast and GOM"="Northwest Atlantic & GOM",
                              "OR"="Oregon",
                              "Pacific stock"="Pacific Ocean",
                              "S Gulf of Maine to upper Bay of Fundy to Gulf of St Lawrence"="S. Gulf of Maine-Upper Bay of Fundy-Gulf of St. Lawrence",
                              "Sable Is"="Sable Islands",
                              "San Diego Bay"="San Diego Bay",
                              "San Juan Islands"="San Juan Islands",
                              "San Miguel Is"="San Miguel Islands",
                              "Scotian Shelf_Canada"="Scotian Shelf, Canada",
                              "South California Bight"="South California Bight",
                              "South East Farallons Island"="Southeast Farallons Island",
                              "Southern CA"="Southern California",
                              "Southern SCB"="Southern SCB",
                              "UUColony_Farallons Island"="UU Colony, Farallons Island",
                              "Virginia to Gulf of St Lawrence"="Virginia-Gulf of St. Lawrence",
                              "WA"="Washington")))

# Fill in gaps
pred_stocks_other$region[pred_stocks_other$stocklong=="Bluefin tuna Total biomass Pacific stock"] <- "Pacific Ocean"
pred_stocks_other$area[pred_stocks_other$stocklong=="Atlantic white-sided dolphin Abundance Estimate "] <- "NW Atlantic"

# Final formatting
pred_stocks_other <- pred_stocks_other %>% 
  mutate(stockid=NA, 
         stocklong_orig=stocklong, 
         stocklong=make.unique(paste0(comm_name, " ", area, " [", n_units, "]"))) %>% 
  select(stocklong, stocklong_orig, stockid, region, area, species, comm_name, nyr, n_units, reference)

# Make sure stocklong is a unique identifier and check completeness
sum(duplicated(pred_stocks_other$stocklong))
sum(duplicated(pred_stocks_other$stocklong_orig))
apply(pred_stocks_other, 2, function(x) sum(is.na(x)))

# Merge predator stocks
#########################################

# Merge predator stocks
pred_stocks_final <- rbind(pred_stocks_ram, pred_stocks_other) %>% 
  mutate(ocean=NA) %>% 
  left_join(select(taxa, species_sub, type), by=c("species"="species_sub")) %>% 
  select(stocklong, stocklong_orig, stockid, ocean, region, area, type, species, everything()) %>% 
  arrange(stocklong)

# Add ocean
regions <- sort(unique(pred_stocks_final$region))
atl.regions <- c("Atlantic Coast", "Atlantic Ocean", "Atlantic Ocean",
                 "Canada East Coast", "Chesapeake Bay", "Gulf of Maine", 
                 "Northwest Atlantic Coast", "NW Atlantic", "NW Atlantic Coast & Gulf of Maine",
                 "US East Coast", "US Southeast and Gulf", "Western Atlantic")
pac.regions <- regions[!regions%in%atl.regions]
pred_stocks_final$ocean <- ifelse(pred_stocks_final$region%in%atl.regions, "NW Atlantic", "NE Pacific")

# Make sure stocklong is a unique identifier and check completeness
sum(duplicated(pred_stocks_final$stocklong))
sum(duplicated(pred_stocks_final$stocklong_orig))
apply(pred_stocks_final, 2, function(x) sum(is.na(x)))

# Update predator time series
#########################################

# Predators: stocklong, stocklong_orig, reference, year, year_orig, n, n_units, catch

# Update predator time series
pred_ts <- pred_ts %>% 
  rename(stocklong_orig=stocklong, year_orig=year) %>% 
  left_join(select(pred_stocks_final, stocklong_orig, stocklong), by="stocklong_orig") %>% 
  mutate(n_units=ifelse(!is.na(n), n_units, NA)) %>% 
  group_by(stocklong) %>% 
    mutate(year=round(year_orig)) %>% 
  select(stocklong, stocklong_orig, reference, year, year_orig, n, n_units, catch)

# Check for duplicated years (rounding didn't work perfectly)
year_check <- data.frame(n=tapply(pred_ts$year, pred_ts$stocklong, function(x) anyDuplicated(x), simplify=T))
year_check$stocklong <- rownames(year_check); rownames(year_check) <- NULL
year_check <- subset(year_check, n!=0)

# The two messed up time series are from:
# Nye JA, Bundy A, Shackell N, Friedland KD, Link JS (2010) 
# Coherent trends in contiguous survey time-series of major ecological and
# commercial fish species in the Gulf of Maine ecosystem. ICES JMS 67: 26-40.

# Fix Little skate Georges Bank [biomass (log-scale)] years
# The time series should extend from 1970-2007 (38 years - Nye et al. 2010)
# However, the time sereis is two years too long (two years double counted?)
# Eliminate first 1971 (1970.553), eliminate second 1997 (1997.085), change second 1995 (1995.398) to 1996
check1 <- subset(pred_ts, stocklong=="Little skate Georges Bank [biomass (log-scale)]")
pred_ts <- pred_ts %>% 
  mutate(year_orig=as.character(year_orig)) %>% 
  filter(!year_orig%in%c("1970.5531", "1997.0854")) %>% 
  mutate(year=ifelse(year_orig=="1995.3976", 1996, year),
         year_orig=as.numeric(year_orig))

# Fix Little skate Gulf of Maine [biomass (log-scale)] years
# The time series should extend from 1970-2007 (38 years - Nye et al. 2010)
# However, the time series in one year too short (missing 2007?)
check2 <- subset(pred_ts, stocklong=="Little skate Gulf of Maine [biomass (log-scale)]")
check2$year <- 1970:2006
pred_ts$year[pred_ts$stocklong=="Little skate Gulf of Maine [biomass (log-scale)]"] <- 1970:2006

# Check again for duplicated years
year_check2 <- data.frame(n=tapply(pred_ts$year, pred_ts$stocklong, function(x) anyDuplicated(x), simplify=T))
year_check2$stocklong <- rownames(year_check2); rownames(year_check2) <- NULL
year_check2 <- subset(year_check2, n!=0)

# Check completeness
apply(pred_ts, 2, function(x) sum(is.na(x)))


# Format diet info data
################################################################################

# Preferred diet proportions (based on Hilborn et al. 2017)
# prey proportions by energetic contribution > by mass > by numbers > by frequency of occurrence

# Lowercase column names
colnames(diets_orig) <- tolower(colnames(diets_orig))

# Add NA values
diets_orig[diets_orig==""] <- NA
diets_orig[diets_orig=="#N/A"] <- NA
diets_orig[diets_orig=="#NAME?"] <- NA

# Format diet info data
diets <- diets_orig %>%
  # Rename columns
  rename(prey_comm_name=prey, pred_comm_name=predator, pred_type=predator_type, 
         pred_stage2=predator_stage_or_length, pred_stage1=predator_stage, 
         n_data=abundance_data,
         year1=year_diet_data_from, month1=month_diet_data_from, month_name1=name.of.month.nb,
         year2=year_diet_data_to, month2=month_diet_data_to, month_name2=name.of.month.nb.1,
         season=season.of.diet.data.nb, season2=season.of.diet.data.with.no.na.nb, 
         years=range.of.diet.data.years.nb, 
         prey_mm_min=prey_size_mm_min, prey_mm_max=prey_size_mm_max, 
         prey_mm_range=prey.size.range.nb, prey_mm_range2=prey.size.range.mm.pasted.nb,
         prey_mm_avg=prey_size_mm_mean, prey_mm_sd=prey_size_mm_sd, prey_mm_med=prey_size_mm_median,
         method_length=measurement_method, 
         prop_diet_by_n=proportion_diet_fracn_mean, prop_diet_by_wt=proportion_diet_fracw_mean,
         prop_diet_by_wt_type=w_or_vol, prop_diet_by_energy=energetic_contributionfrac, 
         prop_diet_by_index=iri, prop_diet_by_occur=fracfo, prop_diet_by_occur_sd=fracfo_sd,
         method_diet=method, region=larger_area) %>% 
  mutate(pred_type=revalue(pred_type, c("invertebrate"="invertebrates"))) %>% 
  # Add prey scientific name
  mutate(prey_comm_name=revalue(prey_comm_name, c("CAMarket squid"="California market squid",
                                                  "Longfin squid"="Longfin inshore squid",
                                                  "Pacific mackerel"="Pacific chub mackerel",
                                                  "Shortfin squid"="Northern shortfin squid"))) %>% 
  left_join(select(taxa, species_sub, comm_name), by=c("prey_comm_name"="comm_name")) %>% 
  rename(prey_species=species_sub) %>% 
  # Add predator scientific name
  mutate(pred_comm_name=trimws(pred_comm_name),
         pred_comm_name=revalue(pred_comm_name, c("Artic loon"="Arctic loon",
                                                  "Black-legged kittiwakes"="Black-legged kittiwake",
                                                  "Black-vented Shearwater"="Black-vented shearwater",
                                                  "Bluefin tuna"="Pacific bluefin tuna", # confirmed all Pacific
                                                  "Bonapartes gull"="Bonaparte's gull",
                                                  "CA Sea Lion"="California sea lion",
                                                  "CABrown pelican"="California brown pelican",
                                                  "Common dolphin"="Short-beaked common dolphin", # confirmed Short-beaked common dolphin
                                                  "Common murre"="Common guillemot",
                                                  "Curlfin sole_turbot"="Curlfin sole",
                                                  "Dalls porpoise"="Dall's porpoise",
                                                  "Gray seal"="Grey seal",
                                                  "Harbor seal"="Western Atlantic harbor seal", # confirmed all Western Atlantic
                                                  "Humboldt_jumbo_squid"="Humboldt squid",
                                                  "Lincod"="Lingcod",
                                                  "Long-finned and Short-finnd pilot whale"="Long-finned and short-finned pilot whales",
                                                  "North pacific albacora"="Albacore tuna", # no Pacific sub-species
                                                  "Shortfin squid"="Northern shortfin squid",
                                                  "Pacific bonito"="Eastern Pacific bonito",
                                                  "Pacific mackerel"="Pacific chub mackerel",
                                                  "Pink-footed  shearwater"="Pink-footed shearwater",
                                                  "Sailfish"="Indo-Pacific sailfish",
                                                  "Sea raven"="Atlantic sea raven",
                                                  "Smooth hammerhead"="Smooth hammerhead shark",
                                                  "Smooth Hammershead shark"="Smooth hammerhead shark",
                                                  "Steller Sea Lion"="Steller sea lion",
                                                  "White-sided dolphin"="Atlantic white-sided dolphin", # confirmed all Atlantic
                                                  "Wreckfish"="Atlantic wreckfish",
                                                  "Xantuss murrellet"="Xantus's murrellet"))) %>%
  left_join(select(taxa, species_sub, comm_name), by=c("pred_comm_name"="comm_name")) %>% 
  rename(pred_species=species_sub) %>% 
  # Format predator stage
  # Predator stage 2 has not been rigorously formatted
  mutate(pred_stage1=tolower(trimws(pred_stage1)),
         pred_stage1=revalue(pred_stage1, c("adults"="adult", "chicks"="chick")),
         pred_stage2=tolower(trimws(pred_stage2))) %>% 
  # Format other columns
  # Length/diet methods not rigorously formatted
  mutate(ocean=NA,
         years=ifelse(years=="0", NA, years),
         method_length=trimws(tolower(method_length)),
         method_diet=trimws(tolower(method_diet)),
         # Format region
         region=trimws(region),
         region=revalue(region, c("AK"="Alaska",
                                  "BC"="British Columbia",
                                  "BC_WA"="BC/WA",
                                  "CA"="California",
                                  "CA_MEX"="CA/Mexico",
                                  "CA_OR"="CA/OR",
                                  "CA_OR_WA"="CA/OR/WA",
                                  "CA_OR_WA_BC"="CA/OR/WA/BC",
                                  "CA_WA"="CA/WA",
                                  "Chesapeake bay"="Chesapeake Bay", 
                                  "GOC"="Gulf of California",
                                  "MEX"="Mexico",
                                  "Mid Atlantic"="Mid-Atlantic",
                                  "Mid Atlantic Bight"="Mid-Atlantic",
                                  "North west Atlantic"="NW Atlantic",
                                  "North West Atlantic"="NW Atlantic",
                                  "OR"="Oregon", 
                                  "OR_WA"="OR/WA",
                                  "OR_WA_BC"="OR/WA/BC",
                                  "Scotian shelf"="Scotian Shelf",
                                  "WA"="Washington",
                                  "WA_BC"="WA/BC")),
         # Format areas
         area=gsub("([a-z])([A-Z])", "\\1 \\2", area),
         area=gsub("_TO_|_To_", "-", area),
         area=gsub("From |Off ", "", area),
         area=gsub("Is$", "Island", area),
         area=gsub("_", "/", area),
         area=revalue(area, c("GOC"="Gulf of California",
                              "Mid Atlantic Bight"="Mid-Atlantic Bight",
                              "Atlantic coast"="Atlantic Coast",
                              "Baja Cal"="Baja California",
                              "Baja Cal/GOC"="Baja California/Gulf of California",
                              "Bull Creek/South Carolina"="Bull Creek-South Carolina",
                              "Celtic sea"="Celtic Sea",
                              "Chesapeake bay"="Chesapeake Bay",
                              "Midshelf/Farallon Gulf"="Midshelf Farallon Gulf",
                              "New Jersey/to/Massachusset Coast"="New Jersey-Massachusset Coast",
                              "Norhern California Current"="Northern California Current",
                              "Scotian shelf"="Scotian Shelf",
                              "South Carolina/Estuarine"="South Carolina-Estuarine",
                              "South Carolina/Oceanic"="South Carolina-Oceanic")),
         area=trimws(area),
         # Format references
         reference=gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", reference),
         reference=gsub("etal", " et al.", reference),
         reference=gsub("and", " and ", reference),
         reference=gsub("citedby", " cited by ", reference),
         reference=gsub("([a-z])([A-Z])", "\\1 \\2", reference)) %>% 
  # Rearrange columns
  select(pred_type, pred_comm_name, pred_species, 
         ocean, region, area, reference, prey_species, prey_comm_name, n_data,
         years, season, year1, month1, month_name1, year2, month2, month_name2,
         method_length, method_diet, comment, n_samples, pred_stage1, pred_stage2,
         prey_mm_min, prey_mm_max, prey_mm_range, prey_mm_med, prey_mm_avg, prey_mm_sd,
         prop_diet_by_wt, prop_diet_by_wt_type, prop_diet_by_n, prop_diet_by_energy,
         prop_diet_by_occur, prop_diet_by_occur_sd, prop_diet_by_index, everything()) %>% 
  # Remove columns
  select(-c(season2, prey_mm_range2))

# Inspect area
sort(unique(diets$area))

# Fill in missing areas
diets.missing.area <- subset(diets, is.na(area))
# For rows with region but no area, use region as area
diets$area[!is.na(diets$region) & is.na(diets$area)] <- diets$region[!is.na(diets$region) & is.na(diets$area)]
# Look up areas for the following:
# PerezandBigg1986 - Northern fur seal: western North America
# MasonandBishop2001 - Jack mackerel: California
# NicholsandBreder1927citedbyBowman1984 - Silver hake: Northwest Atlantic
diets$area[diets$reference=="Perez and Bigg 1986"] <- "Western North America"
diets$area[diets$reference=="Mason and Bishop 2001"] <- "California"
diets$area[diets$reference=="Nichols and Breder 1927 cited by Bowman 1984"] <- "Northwest Atlantic"

# Fill in missing regions
areas.missing.regions <- sort(unique(diets$area[is.na(diets$region)]))
europe.areas <- "Celtic Sea"
can.east.areas <- "Scotian Shelf"
us.west.areas <- "California"
us.east.areas <- c("Bull Creek-South Carolina", "Charleston Bump-GA Coast", "Florida", "Northwest Atlantic",
                   "North Carolina-Nova Scotia", "Cape Hatteras-Virginia", "Georges Bank", 
                   "Georges Bank-S New England", "Gulf of Maine", "Long Island bays", "Mid-Atlantic Bight", 
                   "North Carolina Coast", "North Carolina", "North Carolina", "South New England", 
                   "South Carolina-Estuarine", "South Carolina-Oceanic", "Southern New England", "Western North America")
diets$region[diets$area%in%us.west.areas & is.na(diets$region) & !is.na(diets$area)] <- "US West Coast"
diets$region[diets$area%in%us.east.areas & is.na(diets$region) & !is.na(diets$area)] <- "US East Coast"
diets$region[diets$area%in%europe.areas & is.na(diets$region) & !is.na(diets$area)] <- "Europe"
diets$region[diets$area%in%can.east.areas & is.na(diets$region) & !is.na(diets$area)] <- "Canada East Coast"

# Add ocean column
regions <- sort(unique(diets$region))
atl.regions <- c("Bay of Fundy", "Chesapeake Bay", "Grand Banks of Newfoundland", "Gulf of Maine", "Gulf of Mexico",
                 "Mid-Atlantic", "New England", "Newfoundland", "North Atlantic Bight", "NW Atlantic", "Scotian Shelf",
                 "US East Coast", "Canada East Coast")
pac.regions <- regions[!regions%in%atl.regions]
diets$ocean <- ifelse(diets$region%in%atl.regions, "NW Atlantic", "NE Pacific")  
diets$ocean[diets$region=="Europe"] <- "Europe"

# Inspect
str(diets)
table(diets$pred_type)
table(diets$pred_stage1)
table(diets$pred_stage2)
table(diets$ocean)
table(diets$method_diet)

# Update Spiny dogfish in the Pacific
diets$pred_comm_name[diets$pred_species=="Squalus acanthias" & diets$ocean=="NE Pacific"] <- "Spotted spiny dogfish"
diets$pred_species[diets$pred_species=="Squalus acanthias" & diets$ocean=="NE Pacific"] <- "Squalus suckleyi"

# Are the season/range fields identical? Yes.
# Note: this only works when the columns have been retained (I removed them after checking)
sum(diets$season!=diets$season2, na.rm=T)
sum(diets$prey_mm_range!=diets$prey_mm_range2, na.rm=T)

# Check completeness
# Pred type to prey common name should be 0
apply(diets, 2, function(x) sum(is.na(x)))


# Format prey importance data
################################################################################

# Format prey importance data
colnames(prey_impt_orig) <- tolower(colnames(prey_impt_orig))
prey_impt <- prey_impt_orig %>%
  # Rename columns
  rename(pred_stock_orig=predator, impt=importance, impt_avg=importancemean, area=region,
         prey_name=prey, prey_stock_orig=preyinabund, pred_type=predator_type, pred_comm_name=predname) %>% 
  # Format predator columns
  mutate(pred_type=revalue(pred_type, c("invertebrate"="invertebrates")),
         pred_comm_name=trimws(pred_comm_name),
         pred_comm_name=revalue(pred_comm_name, c("Artic loon"="Arctic loon",
                                                  "Black-legged kittiwakes"="Black-legged kittiwake",
                                                  "Black-vented Shearwater"="Black-vented shearwater",
                                                  "Bluefin tuna"="Pacific bluefin tuna", # confirmed all Pacific
                                                  "Bonapartes gull"="Bonaparte's gull",
                                                  "CA Sea Lion"="California sea lion",
                                                  "CABrown pelican"="California brown pelican",
                                                  "Common dolphin"="Short-beaked common dolphin", # confirmed Short-beaked common dolphin
                                                  "Common murre"="Common guillemot",
                                                  "Curlfin sole_turbot"="Curlfin sole",
                                                  "Dalls porpoise"="Dall's porpoise",
                                                  "Gray seal"="Grey seal",
                                                  "Harbor seal"="Western Atlantic harbor seal", # confirmed all Western Atlantic
                                                  "Humboldt_jumbo_squid"="Humboldt squid",
                                                  "Lincod"="Lingcod",
                                                  "Long-finned and Short-finnd pilot whale"="Long-finned and short-finned pilot whales",
                                                  "North pacific albacora"="Albacore tuna", # no Pacific sub-species
                                                  "Shortfin squid"="Northern shortfin squid",
                                                  "Pacific bonito"="Eastern Pacific bonito",
                                                  "Pacific mackerel"="Pacific chub mackerel",
                                                  "Pink-footed  shearwater"="Pink-footed shearwater",
                                                  "Sailfish"="Indo-Pacific sailfish",
                                                  "Sea raven"="Atlantic sea raven",
                                                  "Smooth hammerhead"="Smooth hammerhead shark",
                                                  "Smooth Hammershead shark"="Smooth hammerhead shark",
                                                  "Steller Sea Lion"="Steller sea lion",
                                                  "White-sided dolphin"="Atlantic white-sided dolphin", # confirmed all Atlantic
                                                  "Wreckfish"="Atlantic wreckfish",
                                                  "Xantuss murrellet"="Xantus's murrellet"))) %>%
  left_join(select(taxa, comm_name, species), by=c("pred_comm_name"="comm_name")) %>% 
  rename(pred_species=species) %>% 
  # Add prey stock info
  left_join(select(prey_stocks, stocklong_orig, stocklong, species, comm_name), by=c("prey_stock_orig"="stocklong_orig")) %>% 
  select(-prey_name) %>% 
  rename(prey_stock=stocklong, prey_comm_name=comm_name, prey_species=species) %>% 
  # Rearrange columns
  select(pred_stock_orig, pred_type, pred_comm_name, pred_species, area, prey_stock, prey_comm_name, prey_species, impt, impt_avg)

# Check completeness
apply(prey_impt, 2, function(x) sum(is.na(x)))


# Export data
################################################################################

# Export data
write.csv(taxa, paste(outputdir, "hilborn_etal_2017_taxa_info.csv", sep="/"), row.names=F)
write.csv(prey_stocks, paste(outputdir, "hilborn_etal_2017_prey_stocks.csv", sep="/"), row.names=F)
write.csv(pred_stocks_final, paste(outputdir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), row.names=F)
write.csv(prey_ts, paste(outputdir, "hilborn_etal_2017_prey_time_series.csv", sep="/"), row.names=F)
write.csv(pred_ts, paste(outputdir, "hilborn_etal_2017_predator_time_series.csv", sep="/"), row.names=F)
write.csv(prey_impt, paste(outputdir, "hilborn_etal_2017_prey_importance.csv", sep="/"), row.names=F)
write.csv(diets, paste(outputdir, "hilborn_etal_2017_diet_information.csv", sep="/"), row.names=F)





