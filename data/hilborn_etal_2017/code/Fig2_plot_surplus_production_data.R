
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
outputdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/output"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017/figures"

# Read data
data <- read.csv(paste(datadir, "hilborn_etal_2017_time_series_to_use.csv", sep="/"), as.is=T)
pred_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)

# Helper functions
################################################################################


# Fit surplus production model
fit.sp.model <- function(sp, ssb){
  r_start <- 0.4
  k_start <- max(ssb) * 1.53
  spfit <- try(nls(sp ~ r*ssb*(1-ssb/k),
                   algorithm="port", lower=c(r=0,k=0), 
                   start=list(r=r_start, k=k_start)))
  return(spfit)
}

# Plot surplus production curves
################################################################################

# Reduce and format data
stocks <- unique(data$stocklong)
pred_stocks <- pred_stocks %>% 
  filter(stocklong %in% stocks) %>% 
  mutate(r=NA, k=NA) %>% 
  arrange(desc(ocean))

# Setup figure
figname <- "Fig2_pred_surplus_production.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(5,4), mar=c(3.5,1,0.1,0.5), mgp=c(3.3,1,0), oma=c(4,4,2,2))

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
  plot(sp ~ n, sdata, bty="n", type="p", las=2, lwd=0.8, col="black", cex.axis=0.9,
       xlim=c(0,xmax), ylim=c(ymin, ymax), xlab="", ylab="", xaxt="n", yaxt="n")
  axis(1, at=c(0,xmax), cex.axis=0.9)
  axis(2, at=c(ymin,ymax), cex.axis=0.9)
  
  # Label stock
  stock_text <- paste(spp, paste0(area, " (", n_units, ")"), sep="\n")
  text(x=0, y=ymax*0.9, pos=4, labels=stock_text, font=2, cex=0.8, col="black")
  
  # Fit and plot surplus production
  spfit <- fit.sp.model(ssb=sdata$n, sp=sdata$sp)
  if(!(inherits(spfit, "try-error"))){
    r <- coef(spfit)[1]
    k <- coef(spfit)[2]
    pred_stocks$r[i] <- r
    pred_stocks$k[i] <- k
    curve(r*x*(1-x/k), from=0, to=xmax, add=T, lty=1, lwd=0.9, col="black")
  }
  
}

# Add y-axis label
mtext("Biomass", outer=T, side=1, adj=0.5, line=2, cex=0.8)
mtext("Surplus production", outer=T, side=2, adj=0.53, line=2, cex=0.8)

# Off
dev.off()
graphics.off()

# Export NLS fits
print(paste0(sum(!is.na(pred_stocks$r)), " of ", nrow(pred_stocks), " fit"))
write.csv(pred_stocks, paste(outputdir, "hilborn_etal_2017_nls_spmodel_fits.csv", sep="/"))

