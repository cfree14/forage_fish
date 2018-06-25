
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
datadir <- "data/new_data/"
plotdir <- "data/new_data/figures"


# Read data
diets <- read.csv(paste(datadir, "predator_diet_information.csv", sep="/"), as.is=T)
pred_ts <- read.csv(paste(datadir, "predator_time_series.csv", sep="/"), as.is=T)
preds <- read.csv(paste(datadir, "predator_stocks.csv", sep="/"), as.is=T)
taxa <- read.csv(paste(datadir, "taxanomic_key.csv", sep="/"), as.is=T)


# Plot predator time series
################################################################################

# Setup figure
figname <- "Fig1_pred_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6,4), oma=c(2,4,2,2), mar=c(2,2,2,0.5))

# Loop through predators
for(i in 1:nrow(preds)){
  
  # Subset data
  pred <- preds$stocklong[i]
  stockshort <- paste(preds$location[i], preds$pred_comm[i])
  sdata <- subset(pred_ts, stocklong==pred)
  
  # Plot data
  plot(n ~ year, sdata, type="l", bty="n", las=1, 
       xlab="", ylab="", main=stockshort, cex.main=0.9)
  
}

# Off
dev.off()
graphics.off()




