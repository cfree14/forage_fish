
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(grid)
library(gridExtra)
library(tidyverse)
library(freeR)
library(rnaturalearth)

# Directories
datadir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read data
load(file.path(datadir, "data_composite_final_sst.Rdata"))

# Get countries
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") 

# Format predator stocks
stocks <- stocks %>% 
  mutate(type_label=recode(type, "birds"="Seabird", "fish"="Fish", "mammals"="Marine mammal"))


# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text = element_text(size=6),
                  legend.title = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Map theme
map_theme <- theme(
                   # Add y-axis padding
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.title.y=element_text(color="white", size=8),
                   axis.text.y=element_text(color="white", size=10),
                   plot.title=element_blank(),
                   legend.text = element_text(size=6),
                   legend.title = element_text(size=8),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot map
g1 <- ggplot() +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  geom_point(data=stocks, mapping=aes(x=long_dd, y=lat_dd, color=type_label, size=nprey)) +
  labs(y="Adding space") +
  scale_color_discrete(name="Predator type") +
  scale_size_continuous(name="Number of\nimportant prey") +
  theme_bw() + map_theme
g1


# Predator diet proportions
pred_diets <- stocks %>% 
  dplyr::select(stockid, type_label, prey_prop, prey_impt_prop, prey1_prop) %>% 
  gather(key="prop_type", value="prop_value", 3:5) %>% 
  mutate(prop_value=pmin(prop_value, 1),
         prop_type=recode(prop_type, 
                          "prey_prop"="All prey", 
                          "prey_impt_prop"="Critical prey only\n(>10% of diet)",
                          "prey1_prop"="Primary prey only"),
         prop_type=factor(prop_type, levels=c("All prey", "Critical prey only\n(>10% of diet)", "Primary prey only")))

# Plot
g2 <- ggplot(pred_diets, aes(x=prop_value, fill=type_label)) +
  geom_density(alpha=0.5) +
  facet_grid(~prop_type) +
  expand_limits(x=0) +
  labs(x="Proportion of diet", y="Density") +
  scale_fill_discrete(name="Predator type") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.1, 0.76),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# Merge plots
g <- grid.arrange(g1, g2, ncol=1)

# Export
ggsave(g, filename=file.path(plotdir, "figure_predator_map.png"), 
       width=6.5, height=6, units="in", dpi=600)






