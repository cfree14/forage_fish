
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read model estimates
results <- read.csv(file.path(outputdir, "model_results.csv"), as.is=T)


# Plot histograms
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Format data
data <- results %>% 
  select(dataset, framework, covariate, stockid, r, k, betaT, sigmaP) %>% 
  gather(key="parameter", value="value", 5:8) %>% 
  filter(dataset=="composite") %>% 
  mutate(covariate=recode_factor(covariate, 
                                 "composite"="Composite prey\nabundance",
                                 "primary"="Primary prey\nabundance",
                                 "sst"="Sea surface\ntemperature (SST)"),
         parameter=recode_factor(parameter,
                                 "r"="Intrinsic growth rate, r",
                                 "k"="Carrying capacity, K\n(prop. of max abundance)",
                                 "betaT"="Influence on\nproductivity, θ",
                                 "sigmaP"="Residual process\nvariability, σ"))

# Count carrying capacity penalty frequency
k_pen_freq <- data %>% 
  filter(parameter=="Carrying capacity, K\n(prop. of max abundance)") %>% 
  group_by(framework, covariate) %>% 
  summarize(n=sum(value>=5))

# Vertical line key
vline_key <- data %>% 
  select(covariate, parameter) %>% 
  unique() %>% 
  mutate(vline=ifelse(parameter=="Carrying capacity, K\n(prop. of max abundance)", 5, NA))

# Plot fixed effects parameters
g <- ggplot(data %>% filter(framework=="fixed"), aes(x=value)) +
  facet_grid(covariate ~ parameter, scales="free") +
  geom_histogram() +
  # Vertical line
  geom_vline(data=vline_key, mapping=aes(xintercept = vline), linetype="dotted") +
  # Labels
  labs(x="Number of populations", y="Count", title="Fixed effects models") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS7_fixed_effects_model_param_hists.png"), 
       width=6.5, height=5, units="in", dpi=600)

# Plot random effects parameters
g <- ggplot(data %>% filter(framework=="random"), aes(x=value)) +
  facet_grid(covariate ~ parameter, scales="free") +
  geom_histogram() +
  # Vertical line
  geom_vline(data=vline_key, mapping=aes(xintercept = vline), linetype="dotted") +
  # Labels
  labs(x="Parameter value", y="Number of populations", title="Random effects models") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS8_random_effects_model_param_hists.png"), 
       width=6.5, height=5, units="in", dpi=600)








