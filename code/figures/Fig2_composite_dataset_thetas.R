
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "output"
tabledir <- "tables"
plotdir <- "figures"

# Read data
data_orig <- read.csv(file.path(outputdir, "model_results.csv"))


# Composite dataset
################################################################################

# Stock order
# Format data
stockid_order <- data_orig %>% 
  # Composite dataset
  filter(dataset=="composite" & framework=="fixed" & covariate=="composite") %>% 
  arrange(type, desc(betaT))

# Format data
data <- data_orig %>% 
  # Composite dataset
  filter(dataset=="composite") %>% 
  # Format labels
  mutate(covariate=recode(covariate, 
                          "composite"="Composite\nprey abundance",
                          "primary"="Primary\nprey abundance",
                          "sst"="Sea surface\ntemperature"),
         framework=recode(framework, 
                          "fixed"="Fixed effects estimate",
                          "random"="Random effects estimate")) %>% 
  # Sort order
  mutate(stockid=factor(stockid, levels=stockid_order$stockid))

# Data CI
data_ci <- data %>% 
  dplyr::select(stockid, framework, covariate, betaT_inf, betaT_lo, betaT_hi) %>% 
  gather(key="type", value="value", 5:6) %>% 
  mutate(value_cap=pmin(value, 10) %>% pmax(.,-10))
  

# Setup theme
my_theme <- theme(axis.text.y=element_text(size=6),
                  axis.text.x=element_text(size=8),
                  axis.title=element_text(size=10),
                  strip.text = element_text(size=8),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=betaT, y=stockid, col=betaT_inf)) +
  # Add lines
  geom_line(data_ci, mapping=aes(x=value_cap, y=stockid, col=betaT_inf), alpha=0.5) +
  # Add points
  geom_point(size=0.8) +
  facet_grid(framework ~ covariate, scales = "free_x") + 
  # Add vertical line
  geom_vline(xintercept=0, color="grey20") +
  # Add horizontal lines seperating groups
  geom_hline(yintercept=c(11.5,39.5), linetype="dotted", color="grey30") +
  # Constrain xlim
  # xlim(-4,4) +
  # Legend and labels
  scale_color_manual(name="Significance", values=c("red", "grey60", "blue")) +
  labs(x="Influence on predator productivity", y="") +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig2_composite_dataset_infuences.png"), 
       width=6.5, height=7, units="in", dpi=600)



# Primary dataset
################################################################################

# Stock order
# Format data
stockid_order <- data_orig %>% 
  # Composite dataset
  filter(dataset=="primary" & framework=="fixed" & covariate=="primary") %>% 
  arrange(type, desc(betaT))

# Format data
data <- data_orig %>% 
  # Composite dataset
  filter(dataset=="primary") %>% 
  # Format labels
  mutate(covariate=recode(covariate, 
                          "primary"="Primary\nprey abundance",
                          "sst"="Sea surface\ntemperature"),
         framework=recode(framework, 
                          "fixed"="Fixed effects estimate",
                          "random"="Random effects estimate")) %>% 
  # Sort order
  mutate(stockid=factor(stockid, levels=stockid_order$stockid))

# Data CI
data_ci <- data %>% 
  dplyr::select(stockid, framework, covariate, betaT_inf, betaT_lo, betaT_hi) %>% 
  gather(key="type", value="value", 5:6) %>% 
  mutate(value_cap=pmin(value, 10) %>% pmax(.,-10))

# Plot data
g <- ggplot(data, aes(x=betaT, y=stockid, col=betaT_inf)) +
  # Add lines
  geom_line(data_ci, mapping=aes(x=value_cap, y=stockid, col=betaT_inf), alpha=0.5) +
  # Add points
  geom_point(size=0.8) +
  facet_grid(framework ~ covariate) + 
  # Add vertical line
  geom_vline(xintercept=0, color="grey20") +
  # Add horizontal lines seperating groups
  geom_hline(yintercept=c(11.5,39.5), linetype="dotted", color="grey30") +
  # Constrain xlim
  # xlim(-4,4) +
  # Legend and labels
  scale_color_manual(name="Significance", values=c("red", "grey60", "blue")) +
  labs(x="Influence on predator productivity", y="") +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig3_primary_dataset_infuences.png"), 
       width=6.5, height=7, units="in", dpi=600)




