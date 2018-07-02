
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read model data
load(file.path(datadir, "data_final.Rdata"))

# Read model results
stocks <- read.csv(file.path(outputdir, "pella_best_fixed_prey1_results.csv"), as.is=T)


# Plot data
################################################################################

# For y-axis label
top.i <- seq(1, nrow(stocks), 24)

# Setup figure
figname <- "AppendixF_pred_prey_curves.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6, 4), mar=c(2.5, 2.5, 2.5, 0.5), mgp=c(2.5,0.8,0), oma=c(4,6,3,3), lwd=0.8)

# Loop through problem stocks
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- data %>%
    filter(stockid==stock) %>% 
    mutate(tb_sd=tb/max(tb),
           prey1_b_sd=prey1_b/max(prey1_b))

  # Plot data
  colors <- colorpal(brewer.pal(9, "YlGnBu"), nrow(sdata))
  plot(tb_sd ~ prey1_b_sd, sdata, bty="n", las=1,
       type="b", pch=21, bg=colors, cex=1.2,
       xlim=c(0,1), ylim=c(0,1))
  
  # Label first and last points
  lyrs <- summary(sdata$year)[c(1,3,6)]
  ldata <- sdata %>% filter(year%in%lyrs)
  text(x=ldata$prey1_b_sd, y=ldata$tb_sd, labels=ldata$year, pos=2, font=2, xpd=NA)

  
}


# Off
dev.off()
graphics.off()





