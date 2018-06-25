
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
data_orig <- read.csv(paste(datadir, "hilborn_etal_2017_diet_proportions.csv", sep="/"), as.is=T)

# Format data
################################################################################

# Simplify data
sort(unique(data_orig$prey_comm_name))
data <- data_orig %>% 
  rename(pred_name=pred_comm_name) %>% 
  mutate(stock=paste(pred_name, ocean),
         prey_short=revalue(prey_comm_name, c("Atlantic herring"="atl_herring",
                                              "Atlantic mackerel"="atl_mackerel",
                                              "Atlantic menhaden"="atl_menhaden",
                                              "California market squid"="pac_market_squid",
                                              "Gulf menhaden"="atl_gulf_menhaden", 
                                              "Longfin inshore squid"="atl_longfin_squid",
                                                "Northern anchovy"="pac_anchovy", 
                                              "Northern shortfin squid"="atl_shortfin_squid",
                                              "Pacific chub mackerel"="pac_mackerel", 
                                              "Pacific hake"="pac_hake", 
                                              "Pacific sardine"="pac_sardine")))

# Reshape data
data_wide <- dcast(data, ocean+pred_name+stock ~ prey_short, value.var="prop_use")

# Add total column
data_wide$total <- rowSums(data_wide[, 4:ncol(data_wide)], na.rm=T)

# Rename columns and rearrange
data_wide <- data_wide %>% 
  select(ocean, pred_name, stock, total, everything())

# Prey species
prey_spp <- sort(unique(data$prey_short))
atl_prey <- prey_spp[grepl("atl", prey_spp)]
pac_prey <- prey_spp[grepl("pac", prey_spp)]

# Plot data
################################################################################

# Setup figure
figname <- "Fig3_diet_composition_all.png"
png(paste(plotdir, figname, sep="/"), width=10, height=6, units="in", res=600)
par(mfrow=c(1,2), mar=c(4,11,0,0.5), mgp=c(2.8,1,0))

# Plot Atlantic data
###########################################

# Subset Atlantic data
atl_data <- data_wide %>% 
  filter(ocean=="NW Atlantic") %>% 
  select(ocean, pred_name, stock, total, 
         atl_gulf_menhaden, atl_herring, atl_longfin_squid, 
         atl_mackerel, atl_menhaden, atl_shortfin_squid) %>% 
  arrange(total)

# Atlanic matrix
atl_mat <- t(as.matrix(atl_data[, 5:ncol(atl_data)]))
atl_mat[is.na(atl_mat)] <- 0

# Colors
colors <- brewer.pal(n=length(atl_prey), "Set1")

# Plot Atlantic species diets
barplot(atl_mat, beside=F, horiz=T, col=colors, cex.axis=0.8,
        names.arg=atl_data$pred_name, las=2, cex.names=0.6)
abline(v=0.2, lty=3)

# Add legend
legend("bottomright", bty="n",  legend=atl_prey, fill=colors, cex=0.6)


# Plot Pacific data
###########################################

# Subset Pacific data
pac_data <- data_wide %>% 
  filter(ocean=="NE Pacific") %>% 
  select(ocean, pred_name, stock, total, 
         pac_anchovy, pac_hake, pac_mackerel, pac_market_squid, pac_sardine) %>% 
  arrange(total)

# pacanic matrix
pac_mat <- t(as.matrix(pac_data[, 5:ncol(pac_data)]))
pac_mat[is.na(pac_mat)] <- 0

# Colors
colors <- brewer.pal(n=length(pac_prey), "Set2")

# Plot pacantic species diets
barplot(pac_mat, beside=F, horiz=T, col=colors, cex.axis=0.8,
        names.arg=pac_data$pred_name, las=2, cex.names=0.6)
abline(v=0.2, lty=3)

# Add legend
legend("bottomright", bty="n",  legend=pac_prey, fill=colors, cex=0.6)

# Off
dev.off()
graphics.off()



