

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Directories
datadir <- "output"
plotdir <- "figures"

# Read data
data <- read.csv(file.path(datadir, "pella_best_fixed_prey1_results.csv"))


# MS stats
################################################################################

# Number of populations
nrow(data)
table(data$type)

# Number of species
nspp <- data %>%
  group_by(type) %>% 
  summarize(n=n_distinct(species))
sum(nspp$n)


# Plot data
################################################################################

# Remove undescribed props
data1 <- filter(data, prey1_prop!=999)

# Colors
cols <- brewer.pal(3, "Set1")

# Setup figure
figname <- "Fig3_thetas_explanatory_variable.png"
png(file.path(plotdir, figname), width=6.5, height=2.5, units="in", res=600)
par(mfrow=c(1,3), mar=c(3,3,0.5,0.5), mgp=c(1.8,0.7,0), oma=c(0,0.8,0,0))

# A. Primary prey
#################################

# Y-axis limits
range(data1$betaT)

# Loop through types
types <- c("fish", "birds", "mammals")
for(i in 1:length(types)){
  
  # Subset data
  sdata <- filter(data1, type==types[i])

  # Primary prey proportion
  plot(betaT ~ prey1_prop, sdata, type="n", bty="n", las=1, 
       xlim=c(0,1), ylim=c(-2,2),
       col=cols[as.factor(data$type)], pch=16,
       xlab="Primary prey proportion", ylab="")
  
  # Fit model
  lmfit <- lm(betaT ~ prey1_prop, sdata)
  
  # Add shading
  x <- seq(0,1,0.01)
  lmci <- predict(lmfit, data.frame(prey1_prop=x), interval="confidence")
  polygon(x = c(x, rev(x)), y = c(lmci[,"lwr"], rev(lmci[,"upr"])), 
          col=freeR::tcolor(cols[i],0.4), border=NA)
  
  # Add lines
  curve(coef(lmfit)[1]+coef(lmfit)[2]*x, 0, 1, add=T, col=cols[i])
  
  # Add points
  points(x=sdata$prey1_prop, y=sdata$betaT, col=cols[i], pch=16)
  
  # Add zero line
  lines(x=c(0,1), y=c(0,0), lty=3, col="grey40")
  
}

# Add y-axis label
mtext("Prey influence", side=2, adj=0.5, line=2.5)



# Off
dev.off()
graphics.off()

