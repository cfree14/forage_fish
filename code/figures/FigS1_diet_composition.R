
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
datadir <- "data/merged_data"
datadir1 <- "data"
plotdir <- "figures"

# Read data
data <- read.csv(paste(datadir, "predator_diet_proportions_final.csv", sep="/"), as.is=T)


# Stats for MS
################################################################################

# Number of predator species
n_distinct(data$pred_comm)

# Number of predator-region couples
n_distinct(data$dietid)


# Plot data
################################################################################

# Regions
regions <- sort(unique(data$region))

# Loop through regions: i <- 1
for(i in 1:length(regions)){
  
  # Subset data
  reg <- regions[i]
  reg_format <- gsub(" ", "_", tolower(reg))
  sdata <- subset(data, region==reg & !is.na(prop_use))
  
  # Prey species
  prey_spp <- sort(unique(sdata$prey_comm))
  n_prey <- length(prey_spp)
  if(n_prey>1){
    prey_cols <- brewer.pal(n_prey, "Set1")
  }else{
    prey_cols <-  brewer.pal(3, "Set1")[1]
  }
  
  # Transform to wide format (and replace NAs with 0s)
  sdata1 <- dcast(sdata, dietid + region + pred_comm ~ prey_comm, value.var="prop_use", fill=0)
  
  # Calculate and arrange by total
  if(n_prey>1){
    sdata1$total <- rowSums(sdata1[, 4:ncol(sdata1)], na.rm=T)
  }else{
    sdata1$total <- sdata1[,4]
  }
  sdata1 <- arrange(sdata1, total)
  
  # Add whether or not there is abundance data
  sdata1 <- select(sdata1, dietid, region, pred_comm, total, everything())
  
  # Extract and transpose props
  props <- select(sdata1, -c(dietid, region, pred_comm,  total))
  props_t <- t(props)
  
  # Setup figure
  figname <- paste0("FigS", i, "_diet_comp_", reg_format, ".png")
  png(paste(plotdir, figname, sep="/"), width=6, height=8, units="in", res=600)
  par(mfrow=c(1,1), mar=c(4,12,1.5,1), mgp=c(2.2,0.8,0))
  
  # Plot data
  labels <- sdata1$pred_comm
  xmax <- ceiling(max(sdata1$total) / 0.5) * 0.5
  bp <- barplot(props_t, beside=F, horiz=T, col=prey_cols, border="grey20",
                names=labels, las=1, cex.names=0.85, cex.axis=0.9,
                xlim=c(0, xmax), xlab="Proportion of diet", main=reg)
  
  # Add vertical line
  lines(x=c(0.2, 0.2), y=c(0, max(bp)+1), lty=3, lwd=2)
  
  # Add legend
  legend("bottomright", legend=prey_spp, fill=prey_cols, bty="n", cex=0.8, 
         title=expression(bold("Prey species")), title.adj=0.1)
  
  # Off
  dev.off()
  graphics.off()
  
}





















