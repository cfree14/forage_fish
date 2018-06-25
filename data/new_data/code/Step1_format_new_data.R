
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(Hmisc) # capitalize()
library(rfishbase)

# Directories
inputdir <- "data/new_data/orig"
outputdir <- "data/new_data/"

# Read data
pred_ts_orig <- read.csv(paste(inputdir, "Predator Abundance Data NB.csv", sep="/"), as.is=T, na.strings=c(""))
diets_orig <- read.csv(paste(inputdir, "Diet Data NB.csv", sep="/"), as.is=T, na.strings=c(""))


################################################################################
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


################################################################################
# Format diet info
################################################################################

# Check completeness
apply(diets_orig, 2, function(x) sum(!is.na(x)))

# Format diet data
diets <- diets_orig %>%
  # Remove columns
  select(-c(sort_cmf, sort_orig, location, n, n_units, year, stocklong)) %>% 
  # Rename columns
  rename(region=ecoregion, area=country, location=island.group, 
         pred_comm=pred_common, pred_species=pred_scientific, 
         reference=source, year=year_orig,
         pred_stage=predator.stage, stage_units=stage.units,
         prey_comm=ff_common, prey_species=ff_scientific, prey_size_mm=ff_eaten_mm,
         prey_age_yr=ff_eaten_yr, diet_prop=ff_diet_perc, diet_prop_units=ff_diet_perc_units, comment=notes) %>% 
  # Reformat diet composition data
  mutate(diet_prop=as.numeric(revalue(diet_prop, c("77-85"=mean(77:85))))/100,
         diet_prop_units=revalue(diet_prop_units, c("% by weight"="by weight",
                                                    "% of stomach content weight"="by weight",
                                                    "% of total weight composition"="by weight",
                                                    "by biomass"="by weight",
                                                    "by mass"="by weight",
                                                    "of stomach weight"="by weight",
                                                    "relative contribution by weight"="by weight",
                                                    "frequency of occurrence"="by freq of occurence",
                                                    "diet matrix"="by ecosystem model")),
         # Reformat other columns
         season=revalue(season, c("July"="Jul", "Breeding"="breeding")), 
         region=revalue(region, c("Norway"="Europe", "North Sea"="Europe", "Barents Sea"="Europe", "Celtic Sea"="Europe")),
         # Reformat predator stage
         pred_stage=revalue(trimws(pred_stage), c("adults"="adult", "chicks"="chick")),
         stage_units=revalue(stage_units, c("age"="yr", "length; cm"="cm", "length; mm"="mm")),
         pred_stage=ifelse(!is.na(pred_stage), paste(pred_stage, stage_units), NA),
         pred_stage=gsub(" age class", "", pred_stage),
         # Reformat prey species
         prey_comm=capitalize(tolower(trimws(prey_comm))), 
         prey_comm=revalue(prey_comm, c("Sprat"="European sprat", "Anchoveta"="Peruvian anchoveta")),
         prey_species=revalue(prey_species, c("Trisopterus esmarki"="Trisopterus esmarkii", "Ammodytidae"="Ammodytidae spp.")),
         # Reformat predator species
         pred_comm=capitalize(tolower(trimws(pred_comm))),
         pred_comm=revalue(pred_comm, c("Puffin"="Atlantic puffin",
                                        "Chub mackerel"="Pacific chub mackerel",
                                        "Fulmar"="Northern fulmar",
                                        "Gannet"="Northern gannet",
                                        "Great black back gull"="Great black-backed gull",
                                        "Hake"="European hake",
                                        "Shag"="European shag",
                                        "Saithe"="Pollock",
                                        "Herring gull"="European herring gull",
                                        "Guillemot"="Common guillemot",
                                        "Eastern pacific bonito"="Eastern Pacific bonito",
                                        "Kittiwake"="Black-legged kittiwake", # confirmed in Rindorf et al 2000, 
                                        "Black legged kittiwake"="Black-legged kittiwake")), # Confirmed in Mackeinson & Daskilov 2007
         pred_species=revalue(pred_species, c("Aka torda"="Alca torda",
                                              "Fulmarus glaciulis"="Fulmarus glacialis",
                                              "Majidae spp"="Majidae spp.",
                                              "Sula bassana"="Morus bassanus",
                                              "Catharacta skua"="Stercorarius skua",
                                              "Sula variegate"="Sula variegata",
                                              "Sarda chiliensis chiliensis"="Sarda chiliensis")))


  
# Fix some predator species
# Daan (1989) whiting == Merlangius merlangus
# In Muck & Sanchez, "mackerel" means "Pacific chub mackerel"
# Konchina (1982) Feeding of the pacific mackerel, Scomber japonicus (Scombridae), near the Peruvian Coast
# In Muck & Sanchez, "horse mackerel" means "Trachurus murphyi" but will call "Chilean jack mackerel"
diets$pred_species[diets$pred_comm=="Whiting"] <- "Merlangius merlangus"
diets$pred_comm[diets$pred_species=="Scomber japonicus"] <- "Pacific chub mackerel"
diets$pred_comm[diets$pred_species=="Scomber scombrus"] <- "Atlantic mackerel"
diets$pred_species[diets$pred_comm=="European herring gull"] <- "Larus argentatus"
diets$pred_comm[diets$pred_species=="Trachurus murphyi"] <- "Chilean jack mackerel"

# Fix some prey species
diets$prey_comm[diets$prey_species=="Ammodytidae spp."] <- "Sandeel spp."
diets$prey_comm[diets$prey_species=="Ammodytes marinus"] <- "Lesser sand-eel"
diets$prey_species[diets$prey_comm=="Atlantic herring (spring spawning)"] <- "Clupea harengus (spring spawning)"

# Inspect species
prey_species <- arrange(unique(select(diets, prey_comm, prey_species)), prey_species)
pred_species <- arrange(unique(select(diets, pred_comm, pred_species)), pred_species)
# pred_species <- arrange(unique(select(diets, pred_comm, pred_species, reference)), pred_species)

# All species in FishBase?
pred_spp <- sort(unique(pred_species$pred_species))
pred_spp[!pred_spp%in%taxa_key$species]

# Add diet id
diets$diet_id <- paste(diets$pred_comm, diets$region)
diets <- select(diets, diet_id, everything())

# Inspect data
table(diets$region)
table(diets$pred_stage)
table(diets$stage_units)
table(diets$season)
table(diets$comment)
table(diets$diet_prop_units)

# Check completeness
apply(diets, 2, function(x) sum(is.na(x)))


################################################################################
# Format predator info
################################################################################

# Check completeness
# Delete sort_cmf, sort_orig, location, stocklong,
# category.of.predator, unit.of.category.of.predator, season,
# ff_common, ff_scientific, ff_eaten_mm, ff_eaten_yr, ff_diet_perc, ff_diet_perf_units
apply(pred_ts_orig, 2, function(x) sum(!is.na(x)))

# Year(original) is identical to and more complete than year
sum(pred_ts$year=pred_ts$year_orig, na.rm=T)

# Format predator time series
pred_ts <- pred_ts_orig %>% 
  select(ecoregion, country, island.group, pred_common, pred_scientific,
         source, year_orig, n, n_units, notes) %>% 
  rename(region=ecoregion, area=country, location=island.group, year=year_orig,
         pred_comm=pred_common, pred_species=pred_scientific, reference=source) %>% 
  mutate(pred_species=revalue(pred_species, c("Sula variegate"="Sula variegata")), 
         location=revalue(location, c("6\xa1-14\xa1 S"="6-14°S")),
         region=revalue(region, c("Norway"="Europe", "North Sea"="Europe")),
         n_units=revalue(n_units, c("completed nests"="nests, completed",
                                    "max number of nests"="nests, max number",
                                    "nests with eggs"="nests, with eggs",
                                    "occupied nests"="nests, occupied",
                                    "max number of apparently occupied nests"="nests, max number occupied", 
                                    "breeding pairs"="number of breeding pairs", 
                                    "apparently occupied burrows"="number of occupied burrows",
                                    "population of adults"="number of adults", 
                                    "individuals"="number of individuals")))


# Fix area/location information
###########################################

# Hernyken Island, Røst Archipelago, Lofoten Islands, Norwegian Sea
pred_ts$location[pred_ts$area=="Hernyken"] <- "Hernyken Island"
pred_ts$area[pred_ts$location=="Hernyken Island"] <- "Norwegian Sea"

# Isle of May, Firth of Forth, Scotland
pred_ts$location[pred_ts$area=="Isle of May"] <- "Isle of May"
pred_ts$area[pred_ts$location=="Isle of May"] <- "Scotland"

# Shetland archpelago, Scotland
pred_ts$area[pred_ts$area=="Shetland"] <- "Shetland Islands"

# Vedoy, Norway, Norwegian Sea,
pred_ts$location[pred_ts$area=="Vedoy"] <- "Vedoy"
pred_ts$area[pred_ts$location=="Vedoy"] <- "Norway"

# Shetland Islands Common guillemot > all Shetland Islands colonies (Dunnet and Heubeck 1995)
pred_ts$location[pred_ts$area=="Shetland Islands" & pred_ts$pred_comm=="Common guillemot"] <- "All Shetland Islands colonies"

# UK Coast along North Sea
pred_ts$location[pred_ts$area=="UK and Ireland"] <- "UK coast"
pred_ts$area[pred_ts$location=="UK coast"] <- "England"

# Jahnke et al 1998
# Las poblaciones de aves guaneras y sus relaciones con la abundancia de Anchoveta y la ocurrencia de eventos El Niño en el mar peruano
# The populations of guano birds and their relationships with the abundance of Anchoveta and the occurrence of El Niño events in the Peruvian sea
pred_ts$location[pred_ts$reference=="Jahncke 1998 Figuera 1"] <- "Peruvian Sea"

# IMARPE (Renato)
pred_ts$location[pred_ts$reference=="IMARPE.Renato"] <- "Peru"

# Final additions and checks
###########################################

# Add stocklong
pred_ts <- pred_ts %>% 
  mutate(stocklong=paste0(pred_comm, " ", location, " (", n_units, ")")) %>% 
  select(stocklong, region, area, location, pred_comm, pred_species, reference, year, n, n_units, notes) %>% 
  arrange(stocklong, year)

# Check year
year_check <- data.frame(n=tapply(pred_ts$year, pred_ts$stocklong, function(x) anyDuplicated(x), simplify=T))
year_check$stocklong <- rownames(year_check); rownames(year_check) <- NULL
year_check <- subset(year_check, n!=0)

# Remove and fix "Black-legged kittiwake North Shields and Marsden (nests, with eggs)"
sdata <- subset(pred_ts, stocklong=="Black-legged kittiwake North Shields and Marsden (nests, with eggs)")
sdata.fix <- sdata[seq(1,nrow(sdata),2),]
pred_ts <- pred_ts %>%
  filter(stocklong!="Black-legged kittiwake North Shields and Marsden (nests, with eggs)") %>% 
  rbind(sdata.fix) %>% 
  arrange(stocklong, year) 

# Check year again
year_check <- data.frame(n=tapply(pred_ts$year, pred_ts$stocklong, function(x) anyDuplicated(x), simplify=T))
year_check$stocklong <- rownames(year_check); rownames(year_check) <- NULL
year_check <- subset(year_check, n!=0)

# Check completeness
apply(pred_ts, 2, function(x) sum(is.na(x)))


# Build predator stock key
###########################################

# Format predator info
pred_stocks <- pred_ts %>% 
  group_by(region, area, location, pred_comm, pred_species, stocklong, n_units) %>% 
  summarize(n_yr=sum(!is.na(n)),
            reference=paste(sort(unique(reference)), collapse=", ")) %>% 
  select(stocklong, region, area, location, pred_comm, pred_species, reference, n_units, n_yr)

# Check scientific names
pred_spp <- sort(unique(pred_stocks$pred_species))
pred_spp[!pred_spp%in%taxa_key$species]

# Inspect info
table(pred_stocks$region)
table(pred_stocks$area)
table(pred_stocks$location)
table(pred_stocks$pred_comm)
table(pred_stocks$n_units)

# Check completeness
apply(pred_stocks, 2, function(x) sum(is.na(x)))


################################################################################
# Build taxa key
################################################################################

# 1. Taxanomic info
# type, class, order, family, genus, species, sub-species, comm_name

# Find and merge all unique species
spp1 <- unique(subset(pred_stocks, select=c(pred_comm, pred_species)))
spp2 <- unique(select(diets, pred_comm, pred_species))
spp3 <- unique(select(diets, prey_comm, prey_species))
colnames(spp3) <- c("pred_comm", "pred_species")
spp <- unique(rbind(spp1, spp2, spp3))

# Format taxa
taxa <- spp %>%
  rename(comm_name=pred_comm, species=pred_species) %>% 
  arrange(species) %>% 
  left_join(taxa_key, by="species") %>% 
  mutate(type=NA, species_sub=species) %>% 
  select(type, class, order, family, genus, species, species_sub, comm_name)

# Fix Clupea harengus (spring spawning)
taxa[taxa$species=="Clupea harengus (spring spawning)", 2:5] <- taxa[taxa$species=="Clupea harengus", 2:5]
taxa$species[taxa$species_sub=="Clupea harengus (spring spawning)"] <- "Clupea harengus"

# Fix Ammodytidae spp. and Majidae spp.
taxa[taxa$species=="Ammodytidae spp.", 2:4] <- taxa[taxa$species=="Ammodytes marinus", 2:4]
taxa[taxa$species=="Majidae spp.", 2:4] <- c("Malacostraca", "Decapoda", "Majidae")

# Add type column
taxa$type[taxa$class=="Aves"] <- "birds"
taxa$type[taxa$class=="Mammalia"] <- "mammals"
taxa$type[taxa$class%in%c("Actinopterygii", "Malacostraca")] <- "fish"

# Arrange
taxa <- arrange(taxa, type, species)

# Export data
################################################################################

# Export data
write.csv(diets, paste(outputdir, "predator_diet_information.csv", sep="/"), row.names=F)
write.csv(pred_ts, paste(outputdir, "predator_time_series.csv", sep="/"), row.names=F)
write.csv(pred_stocks, paste(outputdir, "predator_stocks.csv", sep="/"), row.names=F)
write.csv(taxa, paste(outputdir, "taxanomic_key.csv", sep="/"), row.names=F)






