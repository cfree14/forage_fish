

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/output"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/figures"

# Load data
load(paste(datadir, "hilborn_etal_2017_spmodel_prey_link.Rdata", sep="/"))
rm(hess, results.wide, input.data, model, nstocks, stocks, output, params, problem.stocks, sd)

# Read results
results <- read.csv(paste(datadir, "hilborn_etal_2017_spmodel_prey_results.csv", sep="/"), as.is=T)
results <- arrange(results, desc(betaT))


# Plot figure
################################################################################

# Show mu?
show_mu <- F

# Add colors
results$pcolor <- "black"
results$lcolor <- "grey60"
results$pcolor[results$betaT_max<0] <- "red"
results$lcolor[results$betaT_max<0] <- "red"
results$pcolor[results$betaT_min>0] <- "blue"
results$lcolor[results$betaT_min>0] <- "blue"

# Setup figure
figname <- ifelse(show_mu==T, "Fig1_sp_thetas.png", "Fig1_sp_thetas_no_mu.png")
png(paste(plotdir, figname, sep="/"), width=4, height=5, units="in", res=600)
par(mfrow=c(1,1), mar=c(3,1,0.5,0.5), mgp=c(1.9,0.7,0))

# Setup empty plot
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=0.8,
     xlim=c(-1, 1), ylim=c(1,nrow(results)),
     xlab=expression("θ"["prey"]), ylab="")

# Add shading for theta mean
if(show_mu==T){
  mu_theta <- results.df$estimate[results.df$param=="mu_T"]
  mu_theta_se <- results.df$stderror[results.df$param=="mu_T"]
  mu_min <- mu_theta-mu_theta_se
  mu_max <- mu_theta+mu_theta_se
  rect(xleft=mu_min, xright=mu_max, ytop=nrow(results), ybottom=1, border=NA, col="grey80")
  text(x=0, y=nrow(results)-2, labels=paste0("μ = ", round(mu_theta,2)), pos=4, cex=0.7, col="grey30")
}

# Add theta estimates (points) and errors (lines)
sapply(1:nrow(results), function(x) lines(x=c(results$betaT_min[x], results$betaT_max[x]), y=c(x,x), col=results$lcolor[x]))
points(results$betaT, 1:nrow(results), pch=16, cex=0.8, col=results$pcolor)

# Add theta=0 line
lines(x=c(0,0), y=c(1, nrow(results)), lty=3, col="black", lwd=1.5)

# Add positive/negative influence labels
n_pos <- sum(results$betaT_min>0)
n_neg <- sum(results$betaT_max<0)
n_neutral <- nrow(results)-n_pos-n_neg
text_pos <- paste0("Positive influence\n", n_pos, " stocks")
text_neg <- paste0("Negative influence\n", n_neg, " stocks")
text(x=1.05, y=nrow(results)*0.98, labels=text_pos, pos=2, adj=1, cex=0.7, col="blue")
text(x=-1.05, y=1, labels=text_neg, pos=4, adj=0, cex=0.7, col="red")

# Off
dev.off()
graphics.off()

