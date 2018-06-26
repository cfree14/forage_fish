
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
datadir1 <- "data"
datadir2 <- "data/merged_data"
plotdir <- "figures"

# Read data
data <- read.csv(paste(datadir2, "predator_diet_proportions_final.csv", sep="/"), as.is=T)
taxa <- read.csv(paste(datadir2, "taxanomic_info_final.csv", sep="/"), as.is=T)

# Load predator data
load(file.path(datadir1, "fish_predator_data.Rdata"))
fpreds <- pred_stocks
load(file.path(datadir1, "nonfish_predator_data.Rdata"))
nfpreds <- bm_pred_stocks
rm(pred_stocks, bm_pred_stocks, pred_ts, bm_pred_ts)

# Format and merge predator stocks
# This is to mark stocks that have diet data as having abundance data too
preds1 <- fpreds %>% 
  select(stockid, stocklong, region1, dietid, comm_name, species) %>% 
  rename(region=region1)
nfpreds1 <- nfpreds %>% 
  select(stockid, stocklong, region, dietid, comm_name, species)
preds <- rbind.fill(preds1, nfpreds1)


# Stats for MS
################################################################################

# Number of predator species
n_distinct(data$pred_comm)

# Number of predator-region couples
n_distinct(data$dietid)


# Plot data
################################################################################

# Add predator type to diet data
data <- data %>% 
  left_join(select(taxa, type, comm_name), by=c("pred_comm"="comm_name")) %>% 
  mutate(prey_comm=revalue(prey_comm, c("Atlantic herring (spring spawning)"="Atlantic herring (SS)")))

# Region info for plotting
regions <- c("NE Pacific", "NW Atlantic", "Europe", "Humboldt")
regions_labels <- c("USA/Canada West Coast", "USA/Canada East Coast", "Europe", "Humboldt Current")
region_n <- data %>% 
  filter(!is.na(prop_use)) %>% 
  group_by(region) %>% 
  summarize(n=n_distinct(pred_comm))
row_n <- c(62, 62, 24, 24)

# Setup figure
figname <- "AppendixE_diet_composition_data.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
layout(matrix(1:4, ncol=2, nrow=2, byrow=T), heights=c(0.68, 0.32))
par(oma=c(3,3,2,2), mar=c(2,8,0.5,0.5), mgp=c(2,0.6,0))

# Loop through regions: 
i <- 1
for(i in 1:length(regions)){
  
  # Region data
  reg <- regions[i]
  reg_label<- regions_labels[i]
  sdata <- subset(data, region==reg & !is.na(prop_use))
  
  # Prey species and their colors
  prey_spp <- sort(unique(sdata$prey_comm))
  n_prey <- length(prey_spp)
  if(n_prey>1){prey_cols <- brewer.pal(n_prey, "Set2")}else{prey_cols <-  brewer.pal(3, "Set2")[1]}
  
  # Transform to wide format (and replace NAs with 0s)
  # Add total % forage fish in diet and sort by type and total
  sdata1 <- dcast(sdata, type + pred_comm + dietid ~ prey_comm, value.var="prop_use", fill=0)
  if(n_prey==1){
    sdata1$total <- sdata1[, 4:ncol(sdata1)]
  }else{
    sdata1$total <- rowSums(sdata1[, 4:ncol(sdata1)], na.rm=T)
  }
  sdata1 <- arrange(sdata1, type, total)
  
  # Mark whether or not there is abundance data
  sdata1 <- sdata1 %>% 
    mutate(diet_yn=ifelse(dietid %in% preds$dietid, "*", ""))
  
  # Extract and transpose props
  props <- select(sdata1, -c(type, pred_comm, dietid, diet_yn, total))
  props_t <- t(props)
  
  # Label names and colors
  labels <- paste0(sdata1$pred_comm, sdata1$diet_yn)
  label_cols <- ifelse(sdata1$diet_yn=="*", "black", "grey40")
  
  # Buffer with blanks rows to make even number of bars across plot rows
  rows_goal <- row_n[i]
  rows_needed <- rows_goal-ncol(props_t)
  if(ncol(props_t)!=rows_goal){
    props_t <- cbind(props_t, matrix(data=NA, ncol=rows_needed, nrow=nrow(props_t)))
    labels <- c(labels, rep("", rows_needed))
    label_cols <- c(label_cols, rep(NA, rows_needed))
  }

  # Plot data
  xmax <- ceiling(max(sdata1$total) / 0.5) * 0.5
  xbin <- ifelse(xmax<=1, 0.25, 0.5)
  bp <- barplot(props_t, beside=F, horiz=T, 
                col=prey_cols, border=F, lwd=0.1, xaxt="n", space=0,
                names=rep("", length(labels)), las=1, cex.names=0.85, cex.axis=0.9, 
                xlim=c(0, xmax), xlab="", main=reg_label, xpd=NA)
  text(x=0, y=bp, pos=2, labels=labels, col=label_cols, xpd=NA, cex=0.85)
  axis(side=1, at=seq(0,xmax,xbin))

  # Add vertical line
  lines(x=c(0.2, 0.2), y=c(0, max(bp)+1), lty=3, lwd=2, col="grey30")
  
  # Add legend
  legend("bottomright", legend=prey_spp, fill=prey_cols, bty="n", cex=0.9,
         title="", title.adj=0.1)

  

}

# Add axis label
mtext("Proportion of predator diet", outer=T, side=1, adj=0.6, line=0, cex=1)

# Off
dev.off()
graphics.off()

