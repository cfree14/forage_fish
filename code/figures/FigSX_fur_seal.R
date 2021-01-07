
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

# Add type to data
data <- data_orig %>% 
  left_join(stocks %>% select(stockid, type), by="stockid") %>% 
  ungroup() %>% 
  mutate(type=recode_factor(type,
                            "fish"="Fish",
                            "birds"="Seabirds",
                            "mammals"='Mammals')) %>% 
  filter(stockid=="NFURSTGEORGE")

# My theme
my_theme <-  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot predator abundance
g1 <- ggplot(data, aes(x=year, y=tb)) +
  geom_line() +
  # Labels
  labs(x="Year\n ", y="Predator abundance\n(# of males)") +
  # Theme
  theme_bw()  + my_theme 
g1

# Plot prey abundance
g2 <- ggplot(data, aes(x=year, y=prey1_b/1000)) +
  geom_line() +
  # Labels
  labs(x="Year\n ", y="Prey abundance\n(1000s mt)") +
  # Theme
  theme_bw()  + my_theme 
g2

# Plot prey abundance
g3 <- ggplot(data, aes(x=tb, y=sp, fill=prey1_b/1000)) +
  geom_point(pch=21) +
  # Labels
  labs(x="Predator abundance\n(# of males)", y="Predator production\n(# of males)") +
  # Lines
  geom_hline(yintercept=0, linetype="dotted", color="grey30") +
  # Legend
  scale_fill_gradientn(name="Prey\nabundance\n(1000s mt)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barheight = 1.5, barwidth = 0.5)) +
  # Theme
  theme_bw()  + my_theme +
  theme(legend.position=c(0.8, 0.68),
        legend.background = element_rect(fill=alpha('blue', 0)))
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3)

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_fur_seal_walleye_polllock.png"),
       width=6.5, height=2.25, units="in", dpi=600)







