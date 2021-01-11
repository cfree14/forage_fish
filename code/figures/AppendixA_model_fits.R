


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)
library(grid)
library(gridExtra)

# Multipage PDF
# devtools::install_github("guiastrennec/ggplus")
library(ggplus)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read data
load(file.path(datadir, "data_composite_final_sst.Rdata")) 
data_orig <- data
rm(data)

# Read model fits
fits_orig <- read.csv(file.path(outputdir, "model_results.csv"), as.is=T)


# Build data
################################################################################

# Format fits
fits <- fits_orig %>% 
  filter(dataset=="composite" & framework=="fixed" & covariate=="prey1")

# Reduce to data to ones with fits
data <- data_orig %>% 
  filter(stockid %in% fits$stockid)

# Values to build for
zscores <- seq(-2, 2, 1)

# Build lines
i <- 2; j <- 1
for(i in 1:nrow(fits)){
  
  # Parameters
  stockid <- fits$stockid[i]
  print(stockid)
  r <- fits$r[i]
  k <- fits$k[i]
  theta <- fits$betaT[i]
  p <- 0.55
  
  # Loop through 
  b <- seq(0,1,0.01)
  for(j in 1:length(zscores)){
    
    # Calculate production
    zscore <- zscores[j]
    sp <- r/p * b * (1-(b/k)^p) * exp(theta*zscore)
    
    # Record production
    z <- data.frame(stockid=stockid,
                    zscore=zscore,
                    tb_sd=b,
                    sp_sd=sp)
    
    # Merge
    if(j==1){z_all <- z}else{z_all <- rbind(z_all, z)}
    
  }
  
  # Merge
  if(i==1){spfits <- z_all}else{spfits <- rbind(spfits, z_all)}
  
}


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(data, aes(x=tb_sd, y=sp_sd, fill=prey1_b_sd)) +
  facet_wrap(~stockid, scales="free_y", ncol=6) +
  geom_point(pch=21, size=2) +
  # Line
  geom_line(data=spfits, mapping=aes(x=tb_sd, y=sp_sd, color=zscore, group=zscore), inherit.aes = F) +
  # Labels
  lims(x=c(0,1)) +
  labs(x="Abundance (scaled)", y='Surplus production (scaled)', title="Primary prey fixed effects model fits") +
  # Horizontal guide
  geom_hline(yintercept=0, linetype="dotted", color="grey30") +
  # Legend (points)
  scale_fill_gradient2(name="Primary prey\nabundance (scaled)",
                       mid="white", high="navy", low="darkred") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Legend (lines)
  scale_color_gradient2(name="Production resulting from\n±S.D. from abundance mean",
                       mid="grey80", high="navy", low="darkred") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
# g

# Export
ggsave(g, filename=file.path(plotdir, "AppendixA_model_fits.pdf"), 
       width=8.5, height=11, units="in")

# # Plot data
# g <- ggplot(data, aes(x=tb_sd, y=sp_sd, fill=prey1_b_sd)) +
#   geom_point(pch=21, size=2) +
#   # Line
#   geom_line(data=spfits, mapping=aes(x=tb_sd, y=sp_sd, color=zscore, group=zscore), inherit.aes = F) +
#   # Labels
#   labs(x="Abundance (scaled)", y='Surplus production (scaled)', title="Primary prey fixed effects model fits") +
#   # Horizontal guide
#   geom_hline(yintercept=0, linetype="dotted", color="grey30") +
#   # Legend
#   scale_fill_gradient2(name="Primary prey\nabundance (scaled)") +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Theme
#   theme_bw() + my_theme
# 
# # Plot over multiple PDF pages
# pdf(file.path(plotdir, "AppendixA_model_fits.pdf"), width=8.5, height=11)
# gg10 <- facet_multiple(plot=g, facets="stockid", ncol = 4, nrow = 7, scales="free_y")
# dev.off()
