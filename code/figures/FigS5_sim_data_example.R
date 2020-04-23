

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read data
data <- readRDS(file.path(datadir, "sim_data_for_power_analysis.Rds"))

# Read model estimates
results <- read.csv(file.path(outputdir, "model_results.csv"), as.is=T) %>% 
  filter(dataset=="composite" & framework=="fixed" & covariate == "composite")


# Plot figure
################################################################################

# Plot and example stock
sort(unique(data$stockid))
ex_stock <- "ALBANATL"
ex_data <- data %>% 
  filter(stockid==ex_stock & theta!=0 & sigma!=0)
k <- results$k[results$stockid==ex_stock] * max(ex_data$b_obs)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  axis.title.x=element_blank(),
                  plot.title=element_text(size=12),
                  axis.text.x = element_text(angle = 90, vjust = 0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))

# Plot example data
g <- ggplot(ex_data, aes(x=year, y=b_sim/1000, group=iter)) +
  facet_grid(theta ~ sigma) +
  geom_line(col="grey70") +
  # geom_line(mapping=aes(x=year, y=b_obs/1000)) +
  labs(y="Biomass (1000s of mt)", title="North Atlantic Albacore tuna") +
  geom_hline(yintercept=k/1000, linetype="dotted") +
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS5_sim_data_example.png"), 
       width=6.5, height=6, units="in", dpi=600)




