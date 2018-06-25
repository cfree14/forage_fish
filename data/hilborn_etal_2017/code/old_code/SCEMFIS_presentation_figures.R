
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(RColorBrewer)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017/"
plotdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/figures"

# Read data
taxa <- read.csv(paste(datadir, "hilborn_etal_2017_taxa_info.csv", sep="/"), as.is=T)
prey_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_prey_stocks.csv", sep="/"), as.is=T)
pred_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)
prey_ts <- read.csv(paste(datadir, "hilborn_etal_2017_prey_time_series.csv", sep="/"), as.is=T)
pred_ts <- read.csv(paste(datadir, "hilborn_etal_2017_predator_time_series.csv", sep="/"), as.is=T)
prey_impt <- read.csv(paste(datadir, "hilborn_etal_2017_prey_importance.csv", sep="/"), as.is=T)
diets <- read.csv(paste(datadir, "hilborn_etal_2017_diet_information.csv", sep="/"), as.is=T)
props <- read.csv(paste(datadir, "hilborn_etal_2017_diet_proportions.csv", sep="/"), as.is=T)


# Visualize data
################################################################################

# Select stock
spp <- "Albacore tuna"
stock <- pred_stocks$stocklong[pred_stocks$comm_name==spp][1]
stock_ts <- subset(pred_ts, stocklong==stock & !is.na(n) & !is.na(catch))

# Calculate surplus production
# SP(t) = SSB(t+1) - SSB(t) + C(t)
stock_ts <- mutate(stock_ts,
                   sp=c(n[2:n()]-n[1:(n()-1)]+catch[1:(n()-1)],NA))

# Time series
########################################

# Colors
colors <- brewer.pal(3, "Set1")

# Setup figure
figname <- "Fig1_ssb_catch_sp_time_series.png"
png(paste(plotdir, figname, sep="/"), width=6, height=6, units="in", res=600)
par(mfrow=c(3,1), mar=c(2.5,5,0.5,0.5), mgp=c(3.3,1,0))

# Plot biomass over time
xmin <- floor(min(stock_ts$year) / 10) * 10
xmax <- ceiling(max(stock_ts$year) / 10) * 10
nmax <- ceiling(max(stock_ts$n)/1000 / 200) * 200
plot(n/1000 ~ year, stock_ts, type="l", bty="n", las=1,
     xlim=c(xmin, xmax), ylim=c(0, nmax), cex.axis=1.1, cex.lab=1.2,
     xlab="", ylab="Biomass (1000s mt)", lwd=3, col=colors[1])

# Plot catch over time
ymax <- ceiling(max(stock_ts$catch)/1000 / 100) * 100
plot(catch/1000 ~ year, stock_ts, type="l", bty="n", las=1,
     xlim=c(xmin, xmax), ylim=c(0, ymax), cex.axis=1.1, cex.lab=1.2,
     xlab="", ylab="Catch (1000s mt)", lwd=3, col=colors[2])

# Plot production over time
spmax <- ceiling(max(stock_ts$sp, na.rm=T)/1000 / 100) * 100
plot(sp/1000 ~ year, stock_ts, type="l", bty="n", las=1,
     xlim=c(xmin, xmax), ylim=c(0, spmax), cex.axis=1.1, cex.lab=1.2,
     xlab="", ylab="Surplus production (1000s mt)", lwd=3, col=colors[3], xpd=NA)

# Off
dev.off()
graphics.off()


# Surplus production curve
########################################

# Divide data
stock_ts <- subset(stock_ts, !is.na(sp))
sp <- stock_ts$sp / 1000
n <- stock_ts$n / 1000

# Fit surplus production model
r_start <- 0.4
k_start <- max(n) * 1.5
spfit <-nls(sp ~ r*n*(1-n/k),
            algorithm="port", lower=c(r=0,k=0), 
            start=list(r=r_start, k=k_start))
r <- coef(spfit)[1]
k <- coef(spfit)[2]


# Setup figure
draw_curve <- T
figname <- ifelse(draw_curve==T, "Fig2_surplus_production_curve.png", "Fig2_surplus_production_curve_no_line.png")
png(paste(plotdir, figname, sep="/"), width=6, height=6, units="in", res=600)
par(mfrow=c(1,1), mar=c(4,4,0.5,0.5), mgp=c(2.8,1,0))

# Plot production versus biomass
plot(sp ~ n, bty="n", las=1, type="p",  pch=16, col=colors[2],
     xlim=c(0,nmax), ylim=c(0, spmax),
     xlab="Biomass (1000s mt)", ylab="Surplus production (1000s mt)")

# Add surplus production model
if(draw_curve==T){
  curve(r*x*(1-x/k), from=0, to=1200, add=T, lwd=2)
  r_format <- format(round(r, 2), nsmall=2)
  k_format <- format(round(k, 1), nsmall=1)
  r_text <- paste0("r = ", r_format)
  k_text <- paste0("SSB0 (1000s mt) = ", k_format)
  stat_text <- paste(r_text, k_text, sep="\n")
  text(x=0, y=spmax*0.95, pos=4, labels=stat_text, cex=1)
}

# Off
dev.off()
graphics.off()


# Surplus production curve
########################################

# Build prey key
prey_key <- prey_impt %>% 
  filter(pred_comm_name==spp) %>%
  group_by(pred_comm_name, prey_stock, prey_comm_name, prey_species) %>% 
  summarize(n=n()) %>% 
  filter(prey_comm_name!="Northern shortfin squid") %>% 
  select(-n) %>% 
  left_join(select(props, pred_comm_name, prey_comm_name, prop_use, prop_use_type), 
            by=c("pred_comm_name", "prey_comm_name")) %>% 
  mutate(label=paste0(prey_comm_name, " (", round(prop_use*100,1), "%)")) %>% 
  arrange(desc(prop_use))

# Colors
colors <- brewer.pal(nrow(prey_key), "Set1")
lcolors <- rgb(t(col2rgb(colors))/255, alpha=0.7)

# Setup figure
figname <- "Fig3_prey_stock_time_series.png"
png(paste(plotdir, figname, sep="/"), width=6, height=3, units="in", res=600)
par(mfrow=c(1,1), mar=c(2,0.5,0.5,9), mgp=c(2.8,1,0))

# Plot primary prey
plot(n ~ year, prey_ts, subset=stocklong==prey_key$prey_stock[1] & year>=xmin & year<=xmax, 
     bty="n", las=1, type="l", lwd=3, col=lcolors[1],
     xlim=c(xmin, xmax), yaxt="n", xlab="", ylab="")

# Add additional prey
for(i in 2:nrow(prey_key)){
  par(new=T)
  plot(n ~ year, prey_ts, subset=stocklong==prey_key$prey_stock[i] & year>=xmin & year<=xmax, 
       bty="n", las=1, type="l", lwd=3, col=lcolors[i],
       xlim=c(xmin, xmax), xaxt="n", yaxt="n", xlab="", ylab="")
}

# Add labels
# I DON"T KNOW WHY THIS WORKED
# ymin <- min(prey_ts$n[prey_ts$stocklong==prey_key$prey_stock[nrow(prey_key)]])
# ymax <- max(prey_ts$n[prey_ts$stocklong==prey_key$prey_stock[nrow(prey_key)]])
# y <- seq(ymin,ymax/2, length.out = nrow(prey_key))
# text(x=2010, y=y, pos=4, labels=rev(prey_key$label), cex=0.8, col=rev(colors), xpd=NA)

# Add legend
legend(x=2006, y=ymax+600000, legend=prey_key$label, text.col=colors, xpd=NA, bty="n", 
       title=expression(bold("Prey (% of albacore diet)")), title.adj=0.1, cex=0.85, title.col="black")


# Off
dev.off()
graphics.off()





