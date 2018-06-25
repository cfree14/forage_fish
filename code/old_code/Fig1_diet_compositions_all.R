

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/merged_data"
tabledir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/tables"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/figures"

# Read data
data <- read.csv(paste(datadir, "predator_diet_proportions_final.csv", sep="/"), as.is=T)
preds <- read.csv(paste(datadir, "predator_stocks_final.csv", sep="/"), as.is=T)
taxa <- read.csv(paste(datadir, "taxanomic_info_final.csv", sep="/"), as.is=T)

# Preds without diet data
nodiet <- subset(preds, diet_yn=="N")
write.csv(nodiet, paste(datadir, "predators_missing_diet_data.csv", sep="/"), row.names=F)

# Reduce to preds w/ >20 yr of data
preds20yr <- subset(preds, n_yr>=20)

# Build data
################################################################################

# Regions
regions <- sort(unique(data$region))

# Loop through regions: i <- 1
for(i in 1:length(regions)){
  
  # Subset data
  reg <- regions[i]
  reg_format <- gsub(" ", "_", tolower(reg))
  sdata <- subset(data, region==reg)
  
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
  sdata1$n_data <- ifelse(sdata1$dietid %in% preds20yr$dietid, "Y", "N")
  sdata1 <- select(sdata1, dietid, region, pred_comm, n_data, total, everything())

  # Extract and transpose props
  props <- select(sdata1, -c(dietid, region, pred_comm, n_data, total))
  props_t <- t(props)
  
  # Setup figure
  figname <- paste0("Fig1_diet_comp_", reg_format, ".png")
  png(paste(plotdir, figname, sep="/"), width=6, height=8, units="in", res=600)
  par(mfrow=c(1,1), mar=c(4,12,1.5,1), mgp=c(2.2,0.8,0))
  
  # Plot data
  labels <- ifelse(sdata1$n_data=="Y", sdata1$pred_comm, paste0(sdata1$pred_comm, "**"))
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
  
  # Identify and collate predators w/ diet data but no abundance data
  missing.pred.n <- subset(sdata1, n_data=="N", select=c(region, pred_comm, total))
  if(i==1){
    missing.pred.n.all <- missing.pred.n
  }else{
    missing.pred.n.all <- rbind(missing.pred.n.all, missing.pred.n)
  }
  
}

# Expand preds w/out abundance data
missing.pred.n.all1 <- missing.pred.n.all %>% 
  left_join(select(taxa, comm_name, species, type), by=c("pred_comm"="comm_name")) %>% 
  select(type, region, pred_comm, species, total) %>% 
  arrange(type, region, pred_comm)

# Export preds w/out abundance data
write.csv(missing.pred.n.all1, paste(datadir, "predators_missing_abundance_data.csv", sep="/"), row.names=F)






