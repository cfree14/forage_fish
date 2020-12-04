

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(rnaturalearth)

# Directories
plotdir <- "figures"
ramdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/boundaries_final/ramldb_boundaries"
ramdir2 <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/gis_data/"

# World
world_sf <- rnaturalearth::ne_countries(scale="large", returnclass = "sf")

# Read shapefiles
# all_files <- list.files(ramdir)
# all_files[grepl(preds[3], all_files)]
pred_stockids <- c("MACKNEICES", 
                   "WHITNS-VIId", 
                   "POLLNS-VI-IIIa")
pred_assessids <- c("WGMHSA-MACKNEICES-1972-2007-JENNINGS",
                    "WGNSSK-WHITNS-VIId-1989-2012-CHING",
                    "WGNSSK-POLLNS-VI-IIIa-1964-2012-CHING")
mack_sf <- sf::read_sf(dsn=ramdir, layer=pred_assessids[1]) %>% 
  sf::st_transform(crs=st_crs(world_sf))
whit_sf <- sf::read_sf(dsn=ramdir, layer=pred_assessids[2]) %>% 
  sf::st_transform(crs=st_crs(world_sf))
poll_sf <- sf::read_sf(dsn=ramdir, layer=pred_assessids[3]) %>% 
  sf::st_transform(crs=st_crs(world_sf))

# Sandeel areas
sand_sf <- sf::read_sf(dsn=ramdir2, layer="sandeel_areas_erase") %>% 
  # Use
  mutate(use=ifelse(SANDEEL %in% c(1,2,3), "yes", "no"))

# Plot data
################################################################################

# Merge predators
preds_sf <- bind_rows(mack_sf, whit_sf, poll_sf)

# Plot data
g <- ggplot() +
  # Plot world
  geom_sf(data=world_sf, fill="grey80", color="white", lwd=0.2) +
  # Plot sand eel areas
  geom_sf(data=sand_sf, mapping=aes(fill=use), color="black", lwd=0.2) +
  geom_sf_text(data=sand_sf, mapping=aes(label=SANDEEL)) +
  # Crop map
  coord_sf(xlim = c(-20, 20), ylim = c(40, 70)) +
  # Legend
  scale_fill_manual(values=c(NA, "lightblue")) +
  labs(title="ICES North Sea sandeel management areas") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=8),
        axis.title=element_blank(),
        plot.title=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "none")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS10_sandeel_area_map.png"), 
       width=4, height=5.25, units="in", dpi=600)


