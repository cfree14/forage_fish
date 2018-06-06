
# Clear
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(plyr)
library(dplyr)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/figures"

# Read data
data <- read.csv(paste(datadir, "predator_data_formatted.csv", sep="/"), as.is=T, na.strings="")


# FORMAT DATA
################################################################################

# Build stock key
stocks <- as.data.frame(
  data %>%
    group_by(stocklong, country, location, pred_common, pred_scientific) %>%
    summarize(nyr=sum(!is.na(n)))
)

stock20yr <- filter(stocks, nyr>=20)

# Setup figure
figname <- "predator_abundance_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(5,4), mar=c(3.5, 3.5, 2.5, 0.5), mgp=c(2.4,0.8,0), oma=c(1,1,1,1))

# Loop through predator stocks: i <- 2
for(i in 1:nrow(stocks)){
  
  # Subset data
  spp <- stocks$pred_common[i]
  area <- stocks$location[i]
  stock <- stocks$stocklong[i]
  sdata <- subset(data, stocklong==stock)
  n_units <- unique(sdata$n_units)
  print(paste(i, stock))
  
  # Diet info
  ffspp <- paste(sort(unique(sdata$ff_common)), collapse="\n")
  
  # Plot abundance time series
  nyrs <- sum(!is.na(sdata$n))
  if(nyrs >0){
    xmin <- floor(min(sdata$year) / 10) * 10
    xmax <- ceiling(max(sdata$year) / 10) * 10
    ymax <- max(sdata$n, na.rm=T) * 1.1
    plot.type <- ifelse(nyrs>1, "l", "p")
    title <- paste0(spp, "\n", area)
    plot(n ~ year, sdata, bty="n", type=plot.type, main=title, xaxt="n", yaxt="n", pch=16, cex=1.2,
         xlim=c(xmin, xmax), ylim=c(0, ymax), xlab="", ylab=n_units, cex.main=0.95)
    axis(1, at=c(xmin, xmax))
    axis(2, at=c(0, ymax))
    mtext(ffspp, side=1, adj=0.05, line=-1.5, cex=0.6, col="darkgreen")
  }else{
    title <- paste0(spp, "\n", area)
    plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main=title, cex.main=0.95)
    mtext(ffspp, side=1, adj=0.05, line=-1.5, cex=0.6, col="darkgreen")
  }
  
}

# Off
dev.off()
graphics.off()


