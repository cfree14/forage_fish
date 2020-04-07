
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

# Add type to data
data <- data %>% 
  left_join(stocks %>% select(stockid, type)) %>% 
  mutate(type=recode(type, 
                     "birds"="Seabirds", 
                     "fish"="Fish",
                     "mammals"='Marine mammals'))

# Plot histograms
################################################################################

# TS low and hi
ts_stats <- data %>% 
  group_by(stockid, stocklong, type) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyr=n()) %>% 
  arrange(type, desc(nyr))

# Add factor order to data
data <- data %>%
  ungroup() %>% 
  mutate(stockid=factor(stockid, levels=ts_stats$stockid))

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot timeline
range(data$year)
g1 <- ggplot(data, aes(x=year, y=stockid, fill=type)) +
  geom_raster() +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme(axis.title=element_blank(), 
        legend.position = "none")
        #axis.text.x = element_text(angle = 90, vjust = 0.5))
g1

# Histograms
g2 <- ggplot(ts_stats, aes(x=nyr, fill=type)) +
  facet_grid(~type) +
  geom_histogram(binwidth=10) + 
  labs(x="Time series length (yr)", y="Number of populations") +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- grid.arrange(g1, g2, nrow=2, heights=c(0.7, 0.3))

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_time_series_length.png"),
       width=6.5, height=8, units="in", dpi=600)







