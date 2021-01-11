

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(abind)
library(tidyverse)
library(freeR)
library(reshape2)
library(RColorBrewer)

# GIS packages
library(rgdal)
library(raster)
library(maptools)

# NetCDF packages
# ncdf no longer used; RNetCDF also an option
library(ncdf4)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"
sstdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/sst/data/cobe"

# Read data
load(file.path(datadir, "data_final.Rdata"))

# Read RAM stock centroids
ram_centers <- read.csv("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_centroids_areas_fixed.csv", as.is=T)

# Projections
wgs84 <- CRS("+proj=longlat +datum=WGS84")
wgs84_text <- "+proj=longlat +datum=WGS84"


# Add centroids
################################################################################

# Get RAM centers
ram <- ram_centers %>% 
  dplyr::select(assessid, lat_dd, long_dd) %>% 
  mutate(stockid=sapply(assessid, function(x) strsplit(x, "-")[[1]][2]))

# Add RAM centers to data
stocks1 <- stocks %>% 
  left_join(dplyr::select(ram, -assessid), by="stockid") %>% 
  dplyr::select(stockid, stocklong, region, area, location, comm_name, lat_dd, long_dd)

# Export and fill in centroids
write.csv(stocks1, file.path(datadir, "pred_stock_centers_to_fill.csv"), row.names=F)


# Read final centroid data
################################################################################

# Read data
centroids <- read.csv(file.path(datadir, "pred_stock_centers_final.csv"), as.is=T)


# Read and format SST data
################################################################################

# Read data
sst <- nc_open(paste(sstdir,  "sst.mon.mean.nc", sep="/"))

# Inspect data
print(sst) # summary
sst$nvars # number of variables
names(sst) # properties
names(sst$dim) # dimensions
names(sst$var) # variables

# Extract data
#######################################

# Extract coordinates
lat <- sst$dim$lat$vals
lon <- sst$dim$lon$vals
range(lat); range(lon)

# Extract dates
ncatt_get(sst, varid="time", attname="units")
start_date <- as.Date("1891-01-01") # must inspect above
dates_orig <- ncvar_get(sst, varid="time")
dates <- start_date + dates_orig

# Extract SST into array
sst_array <- ncvar_get(sst, varid="sst")

# Convert 0-360 longitude to -180-180 longitude (each cell is 2 degrees)
dim(sst_array)
hemiE <- sst_array[1:180,,]
hemiW <- sst_array[181:360,,]
sst_array1 <- abind(hemiW, hemiE, along=1)
dim(sst_array1)

# Min and maximum temperatures
tmin <- min(sst_array, na.rm=T)
tmax <- max(sst_array, na.rm=T)

# Create SST raster brick
pal <- rev(brewer.pal(11, "RdYlBu"))
sst_brick <- brick(sst_array1, xmn=-180, xmx=180,
                   ymn=-90, ymx=90, transpose=T, crs=wgs84)

# Plot and confirm good projections
plot(sst_brick, 1, xlim=c(-180,180), ylim=c(-90,90), zlim=c(tmin, tmax), main="", col=pal)
points(lat_dd ~ long_dd, centroids, pch=16)


# Calculate SST time series
################################################################################

# Calculate mean monthly SST within stock boundaries (zonal stats)
centroids_sp <- SpatialPointsDataFrame(coords=centroids[,c("long_dd", "lat_dd")], 
                                       proj4string=wgs84, data=centroids) 
  
sst.monthly <- extract(sst_brick, centroids_sp)

# Add dates as temp matrix names
colnames(sst.monthly) <- paste("a", dates, sep="")

# Merge stock ids with temp matrix
sst.monthly.df <- data.frame(stockid=centroids_sp@data$stockid, sst.monthly)

# Convert temp matrix from wide-to-long
mtemp <- melt(sst.monthly.df, id.vars=c("stockid"),
              variable.name="date", value.name="sst_c")

# Convert date to actual date
mtemp$date <- gsub("a", "", mtemp$date)
mtemp$date <- as.POSIXct(strptime(mtemp$date, format="%Y.%m.%d"))
mtemp$year <- as.numeric(format(mtemp$date, "%Y"))
mtemp$month <- as.numeric(format(mtemp$date, "%m"))

# Rearrange columns
mtemp <- subset(mtemp, select=c(stockid, year, month, date, sst_c))
mtemp <- arrange(mtemp, stockid, date)

# Calculate mean annual SST by stock boundary
atemp <- mtemp %>%
  group_by(stockid, year) %>%
  summarize(sst_c=mean(sst_c))

# Calculate mean SST by stock boundary
stemp <- atemp %>% 
  group_by(stockid) %>% 
  summarize(sst_c=mean(sst_c))


# Add SST data to datasets
################################################################################

# Primary prey
######################################################

# Read data
load(file.path(datadir, "data_final.Rdata"))

# Add SST to data
data_orig <- data 
data <- data_orig %>% 
  left_join(atemp, by=c("stockid", "year")) %>% 
  group_by(stockid) %>% 
  mutate(sst_c_sd=sst_c-mean(sst_c),
         sst_c_sd2=scale(sst_c))

# Add centroid to stocks
stocks <- stocks %>% 
  left_join(dplyr::select(centroids, stockid, long_dd, lat_dd), by="stockid")

# Check completeness
freeR::complete(data)
freeR::complete(stocks)

# Export data
save(data, stocks,
     file=file.path(datadir, "data_final_sst.Rdata"))


# Composite prey
######################################################

# Read data
load(file.path(datadir, "data_composite_final.Rdata"))

# Add SST to data
data_orig <- data 
data <- data_orig %>% 
  left_join(atemp, by=c("stockid", "year")) %>% 
  group_by(stockid) %>% 
  mutate(sst_c_sd=sst_c-mean(sst_c),
         sst_c_sd2=scale(sst_c))

# Add centroid to stocks
stocks <- stocks %>% 
  left_join(dplyr::select(centroids, stockid, long_dd, lat_dd), by="stockid")

# Check completeness
freeR::complete(data)
freeR::complete(stocks)

# Export data
save(data, stocks,
     file=file.path(datadir, "data_composite_final_sst.Rdata"))

