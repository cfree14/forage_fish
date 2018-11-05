

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "output"
plotdir <- "figures"

# Read data
data <- read.csv(file.path(datadir, "pella0.55_fixed_prey1_results.csv"), as.is=T)


# Stats for MS
################################################################################

# Number of fish, birds, mammals
table(data$type)
n_distinct(data$species)

# Format data
################################################################################

# Format data
data <- data %>% 
  # Add 95% confidence intervals and colors
  mutate(lcolor="grey60",
         pcolor="black",
         lcolor=ifelse(betaT_hi<0, "red", lcolor),
         pcolor=ifelse(betaT_hi<0, "red", pcolor),
         lcolor=ifelse(betaT_lo>0, "blue", lcolor),
         pcolor=ifelse(betaT_lo>0, "blue", pcolor),
         type=factor(type, levels=c("mammals", "birds", "fish"))) %>% 
  arrange(type, desc(betaT))


# Plot data
################################################################################

# Theta limit
betaT_lim <- 3

# Setup figure
figname <- "Fig1_thetas.png"
png(file.path(plotdir, figname), width=6.5, height=5, units="in", res=600)
par(mar=c(3,17,0.5,0.8), mgp=c(1.8,0.7,0))

# Setup empty plot
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=0.8,
     xlim=c(-1*betaT_lim, betaT_lim), ylim=c(1,nrow(data)),
     xlab="Influence of primary prey", ylab="")

# Add theta estimates (points) and errors (lines)
sapply(1:nrow(data), function(x) lines(x=c(data$betaT_lo[x], data$betaT_hi[x]), y=c(x,x), col=data$lcolor[x]))
points(data$betaT, 1:nrow(data), pch=16, cex=0.8, col=data$pcolor)

# Add theta=0 line
lines(x=c(0,0), y=c(1, nrow(data)), lty=3, col="black", lwd=1.5)

# Add predator labels
text(x=-1*betaT_lim, y=1:nrow(data), labels=data$stocklong, cex=0.75, xpd=NA, pos=2, col=data$pcolor)

# Add prey labels
text(x=betaT_lim, y=1:nrow(data), offset=-1, labels=data$prey1, cex=0.65, xpd=NA, pos=2, col="grey70")

# Off
dev.off()
graphics.off()

