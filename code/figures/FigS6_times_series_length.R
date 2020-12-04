
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

# Add type to data
data <- data_orig %>% 
  left_join(stocks %>% select(stockid, type), by="stockid") %>% 
  ungroup() %>% 
  mutate(type=recode_factor(type,
                            "fish"="Fish",
                            "birds"="Seabirds",
                            "mammals"='Mammals'))

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
g1 <- ggplot(data, aes(x=year, y=stockid, fill=type, alpha=tb_sd)) +
  geom_raster() +
  facet_grid(type~., scales="free_y", space="free_y") +
  # Labels
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_discrete(guide=F) +
  scale_alpha_continuous(name="Proportion\nof maximum abundance") +
  # Theme
  theme_bw() + my_theme + 
  theme(axis.title=element_blank(), 
        legend.position="bottom")
        #axis.text.x = element_text(angle = 90, vjust = 0.5))
g1

# Histograms
g2 <- ggplot(ts_stats, aes(x=nyr, fill=type)) +
  facet_grid(~type) +
  geom_histogram(binwidth=5) + 
  # Labels
  labs(x="Time series length (yearr)", y="Number of populations") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- grid.arrange(g1, g2, nrow=2, heights=c(0.7, 0.3))

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS2_time_series_length.png"),
       width=6.5, height=8, units="in", dpi=600)







