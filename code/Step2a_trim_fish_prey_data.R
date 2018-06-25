

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data"
plotdir <- "figures"

# Read data
load(file.path(datadir, "potential_fish_predator_prey_data.Rdata"))
prey_stocks <- arrange(prey_stocks, stockid)

# Helper functions
################################################################################

# Function to fit surplus production model
fit.sp.model <- function(sp, tb){
  r_start <- log(0.4)
  k_start <- log(max(tb, na.rm=T) * 1.5)
  spfit <- try(nls(sp ~ exp(r)*tb*(1-tb/exp(k)),
                   start=list(r=r_start, k=k_start)))
  return(spfit)
}


# Plot data
################################################################################

# For headers
top.i <- seq(1, nrow(prey_stocks), 6)

# Plot data and trimming decisions
figname <- "AppendixA_prey_data_and_trimming.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6, 5), mar=c(1.5, 1.0, 2.5, 0.5), mgp=c(2.5,0.5,0), oma=c(3,3,3,3), lwd=0.8)

# Loop through stocks
i <- 1
for(i in 1:nrow(prey_stocks)){
  
  # Subset data
  stock <- prey_stocks$stockid[i]
  sdata <- subset(prey_ts, stockid==stock)
  btype <- toupper(prey_stocks$biomass_type[i])
  method <- prey_stocks$method[i]
  print(paste(i, stock))
  
  # Format data
  sdata <- sdata %>% 
    mutate(biomass=biomass/1000,
           catch=catch/1000,
           ssb=ssb/1000,
           tb=tb/1000,
           sp=sp/1000,
           r=r/1E6)

  # Year limits
  tmin <- floor(min(sdata$year)/10)*10
  tmax <- ceiling(max(sdata$year)/10)*10 
  
  # Biology limits
  bmin <- floor(min(sdata$biomass, na.rm=T))
  bmax <- ceiling(max(sdata$biomass, na.rm=T))
  tbmin <- floor(min(sdata$tb, na.rm=T))
  tbmax <- ceiling(max(sdata$tb, na.rm=T))
  ssbmin <- floor(min(sdata$ssb, na.rm=T))
  ssbmax <- ceiling(max(sdata$ssb, na.rm=T))
  smin <- floor(min(sdata$sp, na.rm=T))
  smax <- ceiling(max(sdata$sp, na.rm=T))
  cmin <- floor(min(sdata$catch, na.rm=T))
  cmax <- ceiling(max(sdata$catch, na.rm=T))
  rmin <- floor(min(sdata$r, na.rm=T))
  rmax <- ceiling(max(sdata$r, na.rm=T))
  if(rmax==1){rmax <- ceiling1(max(sdata$r, na.rm=T), 0.1)}
  if(rmax==0.1){rmax <- ceiling1(max(sdata$r, na.rm=T), 0.01)}
  
  # A. Plot biomass
  #######################################
  
  # Plot data
  plot(biomass ~ year, sdata, type="l", bty="n", 
       xlim=c(tmin, tmax), ylim=c(bmin, bmax), yaxt="n",
       xlab="", ylab="", las=2, col="blue")
  axis(2, at=c(bmin, bmax))
  text(x=tmax, y=bmax-(bmax-bmin)*0.05, pos=2, labels=btype, cex=1.1, col="black", xpd=NA)
  title(stock, line=0.1, xpd=NA)
  if(i %in% top.i){title("Biomass (1000s mt)\ntime series", col.main="blue", line=2, xpd=NA)}
  
  # B. Plot recruitment
  #######################################
  
  # Plot data
  if(sum(sdata$r, na.rm=T)>0){
    plot(r ~ year, sdata, type="l", bty="n", 
         xlim=c(tmin, tmax), ylim=c(rmin, rmax), yaxt="n",
         xlab="", ylab="", las=2, col="purple")
    axis(2, at=c(rmin, rmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Recruitment (millions)\ntime series", col.main="purple", line=2, xpd=NA)}
  
  # C. Plot catch
  #######################################
  
  # Plot data
  if(sum(sdata$catch, na.rm=T)>0){
    plot(catch ~ year, sdata, type="l", bty="n", 
         xlim=c(tmin, tmax), ylim=c(cmin, cmax), yaxt="n",
         xlab="", ylab="", las=2, col="darkgreen")
    axis(2, at=c(cmin, cmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Catch (1000s mt)\ntime series", col.main="darkgreen", line=2, xpd=NA)}
    
  
  # D. Plot surplus production
  #######################################
  
  # Plot SP
  if(sum(sdata$sp, na.rm=T)>0 & sum(sdata$tb, na.rm=T)>0){
    
    # Plot data
    plot(sp ~ tb, sdata, type="p", bty="n", col="grey30",
         xlim=c(tbmin, tbmax), ylim=c(smin, smax), xaxt="n", yaxt="n",
         xlab="", ylab="")
    axis(1, at=c(tbmin, tbmax))
    axis(2, at=c(smin, smax))
    text(x=tbmax, y=smax, pos=2, labels=method, cex=1.1, col="black", xpd=NA)
  
    # Fit and plot SP model
    sdata1 <- subset(sdata, !is.na(sp) & !is.na(tb))
    spfit <- fit.sp.model(sp=sdata1$sp, tb=sdata1$tb)
    if(!(inherits(spfit, "try-error"))){
      r <- exp(coef(spfit)["r"])
      k <- exp(coef(spfit)["k"])
      curve(r*x*(1-x/k), from=tbmin, to=tbmax, add=T, lty=1, lwd=0.9, col="black")
    }
    
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Surplus production\n(1000s mt) curve", col.main="black", line=2, xpd=NA)}
  
  
  # E. Plot stock-recruit relationship
  #######################################
  
  # Plot stock-recruit relationship
  if(sum(sdata$r, na.rm=T)>0 & sum(sdata$ssb, na.rm=T)>0){
    plot(r ~ ssb, sdata, type="p", bty="n", col="grey30",
         xlim=c(ssbmin, ssbmax), ylim=c(rmin, rmax), xaxt="n", yaxt="n")
    axis(1, at=c(ssbmin, ssbmax))
    axis(2, at=c(rmin, rmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Stock-recruit\nrelationship", col.main="black", line=2, xpd=NA)}
  
}

# Off
dev.off()
graphics.off()


# Reduce prey data
################################################################################

# Final data
prey_ts1 <- prey_ts %>% 
  filter(!is.na(biomass))

# Sample size
# MUST ALL HAVE >20 YEARS!
prey_n <- prey_ts1 %>% 
  group_by(stockid) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            yrs=paste0(yr1, "-", yr2),
            nyr=n()) %>% 
  arrange(desc(nyr))

# Create final stocks sheet
table(prey_stocks$region)
prey_stocks1 <- prey_stocks %>% 
  rename(region_orig=region) %>% 
  mutate(region=revalue(region_orig, c("Canada East Coast"="USA/Canada East", 
                                       "Europe non EU"="Europe", 
                                       "European Union"="Europe",
                                       "South America"="Humboldt Current", 
                                       "US East Coast"="USA/Canada East", 
                                       "US Southeast and Gulf"="USA/Canada East", 
                                       "US West Coast"="USA/Canada West"))) %>% 
  left_join(prey_n, by="stockid")

# All but catch cols should be 0
complete(prey_stocks1)

# Export data
################################################################################

# Export data
prey_ts <- prey_ts1
prey_stocks <- prey_stocks1
save(prey_ts, prey_stocks,
     file=file.path(datadir, "prey_data.Rdata"))


