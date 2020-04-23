
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

# Read data
results <- readRDS(file.path(outputdir, "power_analysis_results.Rds"))


# Build data
################################################################################

# Subset thetas
thetas <- results %>%
  # Only fits with CIs
  filter(param=="BetaT" & !is.na(est_lo))
  
# Calculate proportions
props <- thetas %>%
  # Group by scenario and calc stats
  group_by(scenario_id, theta, sigma, iter) %>% 
  summarize(nstocks=n_distinct(stockid), 
            nsigpos=sum(est_lo>0),
            psigpos=nsigpos/nstocks) %>% 
  ungroup() %>% 
  # Calculate means by scenario
  group_by(theta, sigma) %>% 
  summarize(n=n(), 
            psigpos=mean(psigpos), 
            psigpos=psigpos*100,
            perc_label=paste0(round(psigpos), "%")) %>% 
  ungroup()


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  legend.title = element_text(size=9),
                  legend.text=element_text(size=7),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Theta estimates
g1 <- ggplot(thetas, aes(x=est, fill=factor(sigma), group=factor(sigma))) +
  geom_density(alpha=0.6, color="grey20", lwd=0.2) +
  facet_wrap(~format(theta, nsmall=2), ncol=2, scale="free_y") +
  geom_vline(mapping=aes(xintercept = theta), lwd=0.3,) +
  # geom_vline(xintercept=0, linetype="dotted", color="grey30") +
  labs(x="Influence of composite prey\non predator productivity", y="Density", tag="A") +
  scale_fill_discrete(name="Process\nvariability") +
  xlim(c(-0.1, 2)) +
  theme_bw() + my_theme +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
g1

# Percent of significance detected
g2 <- ggplot(props, aes(x=sigma, y=theta, fill=psigpos)) +
  geom_raster() +
  # Axis
  scale_y_continuous(breaks=seq(0.25,1,0.25)) +
  # Add labels
  geom_text(mapping=aes(fill=NULL, label=perc_label), color="grey10", size=3) +
  # Labels
  labs(x="Process variability", y="Influence of composite prey\non predator productivity", tag="B") +
  scale_fill_gradientn(name="Percentage of signicant\npositive estimates",
                       colors=rev(RColorBrewer::brewer.pal(n=9, "RdBu")), breaks=seq(80,100,5), limits=c(NA,100)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme +
  theme(legend.position = "bottom") 
g2

# Merge plots
g <- grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_power_analysis.png"), 
       width=7, height=4, units="in", dpi=600)






