

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

# Projections
wgs84 <- CRS("+proj=longlat +datum=WGS84")
wgs84_text <- "+proj=longlat +datum=WGS84"

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
points(lat_dd ~ long_dd, centroids, pch=16, add=T)


# Read and format SST data
################################################################################

# Dates to plot
dates_plot <- paste0(seq(1960, 2010, 10), "-01-01") %>% as.Date()
indices_plot <- which(dates %in% dates_plot)
plot(sst_brick[[indices_plot]])

# Build SST dataframe
sst_df <- sst_brick[[indices_plot]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", as.character(dates_plot))) %>% 
  gather(key="date", value="sst_c", 3:ncol(.)) %>% 
  # Calculate average by cell and then calculate anamoly
  group_by(long_dd, lat_dd) %>% 
  mutate(sst_c_avg=mean(sst_c),
         sst_c_anom=sst_c-sst_c_avg) %>% 
  ungroup() %>% 
  mutate(year = date %>% as.character() %>% ymd() %>% year())

# Get world
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Plot data
g <- ggplot(sst_df, aes(x=long_dd, y=lat_dd, fill=sst_c_anom)) +
  facet_wrap(~year, ncol=3) +
  geom_tile() + 
  # Plot world
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes=F) +
  # Plot centroids
  geom_point(centroids, mapping=aes(x=long_dd, y=lat_dd), inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-85, -60), ylim = c(30, 50)) +
  # Legend
  labs(x="", y="") +
  scale_fill_gradient2(name="SST anomaly (°C)", na.value = NA, low = "navy", high="darkred", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_sst_experience.png"), 
       width=6, height=5, units="in", dpi=600)




