
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017/"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017/figures"

# Read data
data <- read.csv(paste(datadir, "hilborn_etal_2017_time_series_to_use.csv", sep="/"), as.is=T)
taxa <- read.csv(paste(datadir, "hilborn_etal_2017_taxa_info.csv", sep="/"), as.is=T)
prey_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_prey_stocks.csv", sep="/"), as.is=T)
pred_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)
prey_ts <- read.csv(paste(datadir, "hilborn_etal_2017_prey_time_series.csv", sep="/"), as.is=T)
pred_ts <- read.csv(paste(datadir, "hilborn_etal_2017_predator_time_series.csv", sep="/"), as.is=T)
props <- read.csv(paste(datadir, "hilborn_etal_2017_diet_proportions.csv", sep="/"), as.is=T)


# Plot prey time series
################################################################################

# Format prey data
prey_stocks <- arrange(prey_stocks, desc(ocean))
prey_ts$n <- prey_ts$n / 1000

# Colors
colors <- brewer.pal(9, "Blues")[c(9,6)]

# Setup figure
figname <- "Fig1_prey_time_series.png"
png(paste(plotdir, figname, sep="/"), width=8, height=6, units="in", res=600)
par(mfrow=c(3,4), mar=c(3.5,1,0.1,0.5), mgp=c(3.3,1,0), oma=c(0,4,1,0))

# Loop and plot
for(i in 1:nrow(prey_stocks)){
  
  # Subset data
  stock <- prey_stocks$stocklong[i]
  spp <- prey_stocks$comm_name[i]
  area <- prey_stocks$area[i]
  n_units <- prey_stocks$n_units[i]
  sdata <- subset(prey_ts, stocklong==stock & !is.na(n))

  # Plot data
  ymax <- max(sdata$n) * 1.2
  ymax <- ifelse(ymax>1, round(ymax,0), round(ymax, 4))
  color <- ifelse(prey_stocks$ocean[i]=="NW Atlantic", colors[1], colors[2])
  plot(n ~ year, sdata, bty="n", type="l", las=2, lwd=0.8, col=color, cex.axis=0.9,
       xlim=c(1950,2010), ylim=c(0,  ymax), xlab="", ylab="", yaxt="n")
  axis(2, at=c(0, ymax), labels=c("0", ymax), cex.axis=0.9)
  
  # Label stock
  stock_text <- paste(spp, paste0(area, " (", n_units, ")"), sep="\n")
  text(x=1948, y=ymax*0.92, pos=4, labels=stock_text, font=2, cex=0.8, col=color)
  
  # Label biomass units
  
}

# Add legend
plot.new()
legend("topleft", bty="n", legend=c("NW Atlantic", "NE Pacific"), col=colors, lwd=2, cex=1.1)

# Add y-axis label
mtext("Biomass (1000s mt)", outer=T, side=2, adj=0.53, line=2, cex=0.8)

# Off
dev.off()
graphics.off()


# Plot predator time series
################################################################################

# Format predator data
pred_stocks <- pred_stocks %>% 
  filter(stocklong %in% unique(data$stocklong)) %>% 
  arrange(desc(ocean))

# Setup figure
figname <- "Fig2_pred_time_series.png"
png(paste(plotdir, figname, sep="/"), width=8, height=12, units="in", res=600)
par(mfrow=c(7,4), mar=c(3.5,1,0.1,0.5), mgp=c(3.3,1,0), oma=c(0,4,1,0))

# Loop and plot
for(i in 1:nrow(pred_stocks)){
  
  # Subset data
  stock <- pred_stocks$stocklong[i]
  spp <- pred_stocks$comm_name[i]
  area <- pred_stocks$area[i]
  n_units <- pred_stocks$n_units[i]
  sdata <- subset(pred_ts, stocklong==stock & !is.na(n))
  
  # Plot data
  ymax <- max(sdata$n) * 1.2
  ymax <- ifelse(ymax>1, round(ymax,0), round(ymax, 4))
  color <- ifelse(pred_stocks$ocean[i]=="NW Atlantic", colors[1], colors[2])
  plot(n ~ year, sdata, bty="n", type="l", las=2, lwd=0.8, col=color, cex.axis=0.9,
       xlim=c(1950,2010), ylim=c(0,  ymax), xlab="", ylab="", yaxt="n")
  axis(2, at=c(0, ymax), labels=c("0", ymax), cex.axis=0.9)
  
  # Label stock
  stock_text <- paste(spp, paste0(area, " (", n_units, ")"), sep="\n")
  text(x=1948, y=ymax*0.92, pos=4, labels=stock_text, font=2, cex=0.8, col=color)
  
}

# Add legend
# plot.new()
# legend("topleft", bty="n", legend=c("NW Atlantic", "NE Pacific"), col=colors, lwd=2, cex=1.1)

# Add y-axis label
mtext("Biomass", outer=T, side=2, adj=0.53, line=2, cex=0.8)

# Off
dev.off()
graphics.off()


# Plot surplus production curves
################################################################################

# Fit surplus production model
fit.sp.model <- function(sp, ssb){
  r_start <- 0.4
  k_start <- max(ssb) * 1.5
  spfit <- try(nls(sp ~ r*ssb*(1-ssb/k),
                   algorithm="port", lower=c(r=0,k=0), 
                   start=list(r=r_start, k=k_start)))
  return(spfit)
}

# Setup figure
figname <- "Fig3_pred_surplus_production.png"
png(paste(plotdir, figname, sep="/"), width=8, height=12, units="in", res=600)
par(mfrow=c(7,4), mar=c(3.5,1,0.1,0.5), mgp=c(3.3,1,0), oma=c(0,4,1,0))

# Loop and plot
for(i in 1:nrow(pred_stocks)){
  
  # Subset data
  stock <- pred_stocks$stocklong[i]
  spp <- pred_stocks$comm_name[i]
  area <- pred_stocks$area[i]
  n_units <- pred_stocks$n_units[i]
  sdata <- subset(data, stocklong==stock)
  
  # Plot data
  ymin <- pmin(0, min(sdata$sp))
  xmax <- max(sdata$n)
  ymax <- max(sdata$sp) * 1.2
  color <- ifelse(pred_stocks$ocean[i]=="NW Atlantic", colors[1], colors[2])
  plot(sp ~ n, sdata, bty="n", type="p", las=2, lwd=0.8, col=color, cex.axis=0.9,
       xlim=c(0,xmax), ylim=c(ymin, ymax), xlab="", ylab="", xaxt="n", yaxt="n")
  axis(1, at=c(0,xmax), cex.axis=0.9)
  axis(2, at=c(ymin,ymax), cex.axis=0.9)
  
  # Label stock
  stock_text <- paste(spp, paste0(area, " (", n_units, ")"), sep="\n")
  text(x=0, y=ymax*0.9, pos=4, labels=stock_text, font=2, cex=0.8, col=color)
  
  # Fit and plot surplus production
  spfit <- fit.sp.model(ssb=sdata$n, sp=sdata$sp)
  if(!(inherits(spfit, "try-error"))){
    r <- coef(spfit)[1]
    k <- coef(spfit)[2]
    curve(r*x*(1-x/k), from=0, to=xmax, add=T, lty=1, lwd=0.9, col="black")
  }
  
}

# Add y-axis label
mtext("Biomass", outer=T, side=1, adj=0.5, line=2, cex=0.8)
mtext("Surplus production", outer=T, side=2, adj=0.53, line=2, cex=0.8)

# Off
dev.off()
graphics.off()


# Plot diet composition
################################################################################













