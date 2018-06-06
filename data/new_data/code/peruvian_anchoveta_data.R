

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


# Build data
################################################################################

# Peruvian anchoveta
spp <- "Engraulis ringens"
stocks <- stock_key$stockid[stock_key$scientificname==spp]

# Stock data
anch_stocks <- stock_key %>% 
  filter(stockid%in%stocks) %>% 
  select(stockid, stocklong, region, areaid) %>% 
  left_join(select(area_key, country, areaname, areaid), by="areaid") %>% 
  left_join(select(units, stockid, TB, SSB, TL), by="stockid") %>% 
  rename(area=areaname, tb_units=TB, ssb_units=SSB, tl_units=TL) %>% 
  select(stockid, stocklong, region, country, area, tb_units, ssb_units, tl_units)

# Time series data
anch_ts <- values %>% 
  filter(stockid%in%stocks) %>% 
  # Reduce time series: TN and TC empty
  select(assessid, stockid, stocklong, year, TB, SSB, TL) %>% 
  # Rename columns
  rename(tb=TB, ssb=SSB, tl=TL) %>% 
  # Remove years with no TB or SSB
  filter(!is.na(tb) | !is.na(ssb)) 

# Setup figure
figname <- "peruvian_anchoveta_time_series.png"
png(paste(plotdir, figname, sep="/"), width=6, height=6, units="in", res=600)
par(mfrow=c(4, 1), mar=c(2,5,2,0.5), mgp=c(3.5,1,0), oma=c(1,0,0,0))

# Plot data: i <- 1
for(i in 1:length(stocks)){
  # Subset data
  stock <- stocks[i]
  sdata <- subset(anch_ts, stockid==stock)
  # Plot data
  plot(ssb/1000 ~ year, sdata, bty="n", las=1, type="l",
       xlim=c(1960, 2020), xlab="", ylab="SSB (1000s MT)", main=stock)
}
  
# Off
dev.off()
graphics.off()



