
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(grid)
library(gridExtra)
library(tidyverse)

# Directories
datadir <- "data"
outputdir <- "output"
tabledir <- "tables"
plotdir <- "figures"

# Load data
load(file.path(datadir, "prey_data.Rdata"))




# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  axis.title.x=element_blank(),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5))

# Median B/BMSY
prey_ts <- prey_ts %>% 
  group_by(year) %>% 
  mutate(bbmsy_med=median(bbmsy, na.rm=T),
         bbmsy_med_catg=ifelse(bbmsy_med>=1, "Above 1.0", "Below 1.0"))

# Forage fish stock status
g <- ggplot(prey_ts, aes(x=year, y=bbmsy, group=year, fill=bbmsy_med_catg)) +
  # Add overfished rectangle
  geom_rect(aes(xmin=min(prey_ts$year), xmax=max(prey_ts$year), ymin=0, ymax=0.5), 
            fill="grey80", color=NA) +
  # Add boxplots
  geom_boxplot(outlier.size=0.5, lwd=0.3) +
  # Add labels
  geom_hline(yintercept=1) +
  annotate("text", label="Overfished", x=1947.5, y=0.25, hjust=0, vjust=0.5, color="black", size=2) +
  annotate("text", label=expression("B/B"["MSY"]*" target"), x=1947.5, y=1.2, hjust=0, color="black", size=2) +
  # Axis, legends, themes
  scale_y_continuous(breaks=0:7, labels=0:7) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(x="", y=expression("B/B"["MSY"]), title="Forage fish stock status") +
  scale_fill_manual(name=expression("Median B/B"["MSY"]*" is"), values=c("lightblue", "red")) +
  theme_bw() + my_theme + theme(legend.position = "bottom")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS1_forage_fish_stock_status.png"), 
       width=6.5, height=4, units="in", dpi=600)





