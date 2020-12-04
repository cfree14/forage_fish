
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(gridExtra)

# Directories
datadir1 <- "data"
datadir2 <- "data/merged_data"
plotdir <- "figures"

# Read data
data_orig <- read.csv(paste(datadir2, "predator_diet_proportions_final.csv", sep="/"), as.is=T)
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
preds <- bind_rows(preds1, nfpreds1)

# Format data (fix regions)
data <- data_orig %>% 
  mutate(region=recode(region,
                       "Benguela Current"="South Africa", 
                       "California Current"="U.S. West Coast", 
                       "Europe"="Europe", 
                       "Gulf of Alaska"="U.S. West Coast", 
                       "Humboldt"="Humboldt Current", 
                       "NE Pacific"="U.S. West Coast", 
                       "New Zealand"="New Zealand", 
                       "North Sea"="Europe", 
                       "Norwegian Sea" ="Europe",
                       "NW Atlantic"="U.S. East Coast", 
                       "Pribolof Islands"="U.S. West Coast", 
                       "Scotia Sea" ="Antarctica"))

# Stats for MS
################################################################################

# Number of predator species
n_distinct(data$pred_comm)

# Number of predator-region couples
n_distinct(data$dietid)




# Plot data
################################################################################

# Function to plot data
plot_data <- function(region){
  
  # Subset data
  region_do <- region
  sdata <- data %>% 
    # Filter
    filter(region == region_do) %>% 
    # Add taxa
    left_join(taxa %>% select(comm_name, type), by=c("pred_comm"="comm_name")) %>% 
    # Format 
    mutate(type=recode_factor(type, 
                              "mammals"="Marine\nmammals",
                              "birds"="Seabirds",
                              "fish"="Fish")) %>% 
    # Arrange by total prop
    group_by(pred_comm) %>% 
    mutate(prop_total=sum(prop_use, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(type, desc(prop_use))
  
  # Number of studies key
  nrefs_key <- sdata %>% 
    select(type, pred_comm, nrefs, references) %>% 
    unique()
    
  # Plot data
  g <- ggplot(sdata, aes(x=prop_use, y=reorder(pred_comm, prop_total), fill=prey_comm)) + 
    facet_grid(type~., scales="free_y", space="free_y") +
    geom_bar(stat="identity") +
    # Reference lines
    # geom_vline(mapping=aes(xintercept = c(0.1, 0.2, 1), linetype=c("dotted", "dashed", "solid")), inherit.aes=F) +
    # Labels
    labs(x="Proportion of predator diet\n(mean across diet studies)", y="", title=region) +
    # Legend
    scale_fill_discrete(name="Prey species") +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=6),
          axis.title=element_text(size=8),
          legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          strip.text = element_text(size=8),
          plot.title=element_text(size=10),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g
  
  # Return
  return(g)
  
}

# Plot data
usa_w <- plot_data(region="U.S. West Coast")


# Export plots
ggsave(usa_w, filename=file.path(plotdir, "FigS6_diet_props_usa_west.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

use_e <- plot_data(region="U.S. East Coast")

ggsave(usa_e, filename=file.path(plotdir, "FigS6_diet_props_usa_east.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

eur <- plot_data(region="Europe")
hum <- plot_data(region="Humboldt Current")
sa <- plot_data(region="South Africa")
g <-grid.arrange(eur, hum, sa, ncol=1, heights=c(0.5,0.35,0.15))

ggsave(filename=file.path(plotdir, "FigS6_diet_props_non_usa.png"), plot=g, 
       width=6.5, height=6.5, units="in", dpi=600)











