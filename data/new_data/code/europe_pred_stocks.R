

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/productivity/data/ramldb/ramldb_v3.8"
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/new_data"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/new_data/figures"
mergeddir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/merged_data"

# Read keys
stock_key <- read.csv(paste(ramdir, "ramldb_v38_stock.csv", sep="/"), as.is=T)
assessment_key <- read.csv(paste(ramdir, "ramldb_v38_assessment.csv", sep="/"), as.is=T)
assessor_key <- read.csv(paste(ramdir, "ramldb_v38_assessor.csv", sep="/"), as.is=T)
area_key <- read.csv(paste(ramdir, "ramldb_v38_area.csv", sep="/"), as.is=T)

# Read data
values <- read.csv(paste(ramdir, "ramldb_v38_timeseries_values_views.csv", sep="/"), as.is=T)
units <- read.csv(paste(ramdir, "ramldb_v38_timeseries_units_views.csv", sep="/"), as.is=T)
bioparams_vals <- read.csv(paste(ramdir, "ramldb_v38_bioparams_values_views.csv", sep="/"), as.is=T)
bioparams_units <- read.csv(paste(ramdir, "ramldb_v38_bioparams_units_views.csv", sep="/"), as.is=T)

# Missing predators
missing_preds <- read.csv(paste(mergeddir, "predators_missing_abundance_data.csv", sep="/"), as.is=T)


# Build data
################################################################################

# European 
spp <- subset(missing_preds, type=="fish" & region=="Europe" & total>=0.2)$species
stockids <- subset(stock_key, scientificname%in%spp)$stockid

# Stock data
stocks <- stock_key %>% 
  filter(stockid%in%stockids) %>% 
  select(stockid, stocklong, region, areaid) %>% 
  left_join(select(area_key, country, areaname, areaid), by="areaid") %>% 
  left_join(select(units, stockid, TB, SSB, TN, TL, TC), by="stockid") %>% 
  rename(area=areaname, tb_units=TB, ssb_units=SSB, tn_units=TN, tl_units=TL, tc_units=TC) %>% 
  select(stockid, stocklong, region, country, area, tb_units, ssb_units, tn_units, tl_units, tc_units)
stocks[stocks==""] <- NA

# Time series data
ts <- values %>% 
  filter(stockid%in%stockids) %>% 
  # Reduce time series: TN and TC empty
  select(assessid, stockid, stocklong, year, TB, SSB, TN, TL, TC) %>% 
  # Rename columns
  rename(tb=TB, ssb=SSB, tl=TL) %>% 
  # Remove years with no TB or SSB
  filter(!is.na(tb) | !is.na(ssb)) 






