

# Clear workspace
rm(list = ls())

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
data <- read.csv(file.path(datadir, "pella0.55_fixed_prey1_results.csv"), as.is=T)

# Remove outliers
hist(data$betaT)


# Plot data
################################################################################

# Colors
cols <- brewer.pal(3, "Set1")

# Setup figure
figname <- "Fig2_thetas_explanatory_variable.png"
png(file.path(plotdir, figname), width=6, height=3, units="in", res=600)
par(mfrow=c(1,2), mar=c(3,3,0.5,0.5), mgp=c(1.8,0.7,0), oma=c(0,0.8,0,0))

# A. Primary prey
#################################

# Primary prey proportion
plot(betaT ~ prey1_prop, data, bty="n", las=1, 
     xlim=c(0,1), ylim=c(-2,0.5),
     col=cols[as.factor(data$type)], pch=16,
     xlab="Primary prey proportion", ylab="")
lines(x=c(0,1), y=c(0,0), lty=3, col="grey40")

# Add lines
lm_b <- lm(betaT ~ prey1_prop, data, subset=type=="birds")
lm_f <- lm(betaT ~ prey1_prop, data, subset=type=="fish")
curve(coef(lm_b)[1]+coef(lm_b)[2]*x, 0, 1, add=T, col=cols[1])
curve(coef(lm_f)[1]+coef(lm_f)[2]*x, 0, 1, add=T, col=cols[2])

# Add y-axis label
mtext("Prey influence", side=2, adj=0.5, line=2.5)

# Add legend
legend("bottomright", bty="n", cex=0.8,
       pch=16, col=cols, legend=sort(unique(data$type)))

# B. Total prey
#################################

# Total prey proportion
plot(betaT ~ prey_prop, data, bty="n", las=1,
     xlim=c(0,1), ylim=c(-2,0.5),
     col=cols[as.factor(data$type)], pch=16,
     xlab="Total prey proportion", ylab="")
lines(x=c(0,1), y=c(0,0), lty=3, col="grey40")

# Add lines
lm_b <- lm(betaT ~ prey_prop, data, subset=type=="birds")
lm_f <- lm(betaT ~ prey_prop, data, subset=type=="fish")
curve(coef(lm_b)[1]+coef(lm_b)[2]*x, 0, 1, add=T, col=cols[1])
curve(coef(lm_f)[1]+coef(lm_f)[2]*x, 0, 1, add=T, col=cols[2])

# Off
dev.off()
graphics.off()

