
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)
library(grid)
library(gridExtra)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read model estimates
load(file.path(datadir, "data_composite_final_sst.Rdata")) 
data_orig <- data
rm(data)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Correlation between SST and prey
  group_by(stockid, stocklong) %>% 
  summarize(corr_prey1=cor(prey1_b, sst_c),
            corr_preyC=cor(prey_b, sst_c)) %>% 
  ungroup() %>% 
  # Gather
  gather(key="type", value="correlation", 3:ncol(.)) %>% 
  mutate(type=recode(type, 
                     "corr_prey1"="SST and primary prey",
                     "corr_preyC"="SST and composite prey"))


# Plot data
################################################################################

# My theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=correlation)) +
  facet_wrap(~type) +
  geom_histogram(binwidth=0.05, fill="grey80") + 
  # Vertical guide
  geom_vline(xintercept=0, color="grey30") +
  # Limits
  lims(x=c(-1,1)) +
  # Labels
  labs(x="Correlation", y="Number of predator populations") +
  # Theme
  theme_bw() + my_theme
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_prey_sst_correlation.png"),
       width=4.5, height=2.5, units="in", dpi=600)







