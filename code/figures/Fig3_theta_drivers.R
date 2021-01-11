
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
results <- read.csv(file.path(outputdir, "model_results.csv"))

# Load data
load(file.path(datadir, "data_composite_final_sst.Rdata"))
rm(data)

# Build data
################################################################################

# Thetas use
theta_use <- results %>% 
  # Reduce to right framework
  filter(framework=="fixed" & dataset=="composite") %>% 
  # Reduce columns
  select(stockid, covariate, betaT) %>% 
  # Spread thetas and rename
  spread(key="covariate", value="betaT") %>% 
  rename(theta_sst="sst", theta_prey1="primary", theta_cprey="composite")

# Theta influence use
theta_inf_use <- results %>% 
  # Reduce to right framework
  filter(framework=="fixed" & dataset=="composite") %>% 
  # Reduce columns
  select(stockid, covariate, betaT_inf) %>% 
  # Spread thetas and rename
  spread(key="covariate", value="betaT_inf") %>% 
  rename(theta_inf_sst="sst", theta_inf_prey1="primary", theta_inf_cprey="composite")

# Build data
data <- stocks %>% 
  # Add thetas
  left_join(theta_use, by="stockid") %>% 
  left_join(theta_inf_use, by="stockid") %>%
  # Cap diet proportions
  mutate(prey_prop_cap=pmin(prey_prop, 1),
         prey_impt_prop_cap=pmin(prey_impt_prop, 1),
         prey1_prop_cap=pmin(prey1_prop, 1)) %>% 
  filter(!is.na(theta_prey1)) %>% 
  # Rename types
  mutate(type=recode(type, "birds"="Seabird", "fish"="Fish", "mammals"="Marine mammal")) 


data$stocklong[data$theta_inf_cprey=="negative"]

summary(lm(theta_cprey ~ prey_prop_cap, data))
summary(lm(theta_cprey ~ prey_impt_prop_cap, data))
summary(lm(theta_prey1 ~ prey1_prop_cap, data))

spp_stats <- data %>% 
  group_by(type) %>% 
  summarize(npops=n(),
            nspp=n_distinct(species))

# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  plot.title=element_text(size=12),
                  legend.text=element_text(size=7),
                  legend.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot composite prey
g1 <- ggplot(data, aes(x=prey_prop_cap, y=theta_cprey, color=theta_inf_cprey)) +
  geom_point(size=0.7) + 
  labs(x="Proportion of\nforage fish in diet", 
       y="Influence of composite prey\non predator productivity", tag="A") +
  xlim(0,1) +
  # geom_vline(xintercept=0.2, linetype="dotted", color="grey30") +
  # geom_vline(xintercept=0.6, linetype="dotted", color="grey30") +
  geom_smooth(method="lm", color="black", lwd=0.75) +
  annotate("text", label="r=0.07", x=0, y=min(data$theta_cprey), hjust=0, color="grey20", size=2) +
  geom_hline(yintercept=0, lwd=0.5) +
  scale_color_manual(name="", values=c("red", "grey20", "blue")) +
  theme_bw() + my_theme +
  theme(legend.position=c(0.22, 0.87),
        legend.background = element_rect(fill=alpha('blue', 0)), 
        legend.key.size = unit(0.15, "in"))
g1

# Plot composite prey
g2 <- ggplot(data, aes(x=prey_impt_prop_cap, y=theta_cprey, color=theta_inf_cprey)) +
  geom_point(size=0.7) + 
  labs(x="Proportion of\ncritical prey in diet", 
       y="Influence of composite prey\non predator productivity", tag="B") +
  xlim(0,1) +
  # geom_vline(xintercept=0.1, linetype="dotted", color="grey30") +
  geom_smooth(method="lm", color="black", lwd=0.75) +
  annotate("text", label="r=0.08", x=0, y=min(data$theta_cprey), hjust=0, color="grey20", size=2) +
  geom_hline(yintercept=0, lwd=0.5) +
  scale_color_manual(name="", values=c("red", "grey20", "blue")) +
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Plot primary prey
g3 <- ggplot(data, aes(x=prey1_prop_cap, y=theta_prey1, color=theta_inf_prey1)) +
  geom_point(size=0.7) + 
  labs(x="Proportion of\nprimary prey in diet", 
       y="Influence of primary prey\non predator productivity", tag="C") +
  xlim(0,1) +
  # geom_vline(xintercept=0.1, linetype="dotted", color="grey30") +
  scale_color_manual(name="", values=c("red", "grey20", "blue")) +
  geom_smooth(method="lm", color="black", lwd=0.75) +
  annotate("text", label="r=0.03", x=0, y=min(data$theta_prey1), hjust=0, color="grey20", size=2) +
  geom_hline(yintercept=0, lwd=0.5) +
  theme_bw() + my_theme +
  theme(legend.position="none") 
g3

# Merge plots
g <- grid.arrange(g1, g2, g3, ncol=3)

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_drivers_of_predator_influence.png"), 
       width=6.5, height=2.3, units="in", dpi=600)

