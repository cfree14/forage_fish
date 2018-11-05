

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


# Identify years to trim
################################################################################

# Stocks to cut
to.cut <- c("ATHAL5YZ", "BLACKROCKORECOAST", "SPSDOGPCOAST", 
            "BIGEYEATL", "SWORDNATL", "YFINATL") # Tunas too close to SP model

# HMS species now included:
# ALBANATL, ALBANPAC, BIGEYEATL*, PACBTUNA, STMARLINNEPAC, SWORDNATL*, WMARLINATL, YFINATL*

# Identify trim years
# Columns: stock id, biomass year, recruitment year, catch year, 
trim.years <- matrix(data=c("ARFLOUNDPCOAST", 1939, 1964, NA,
                            "ATHAL5YZ", 1898, NA, NA,
                            "BLACKROCKCAL", 1973, 1959, NA,
                            "BLACKROCKWASH", 1960, 1960, NA,
                            "HMACKIIa-IVa-Vb-VIa-VII-VIII", NA, 1983, NA,
                            "LINGCODNPCOAST", 1940, 1960, NA,
                            "LINGCODSPCOAST", NA, 1971, NA,
                            "MONKGOMNGB", 1992, NA, NA,
                            "SABLEFPCOAST", NA, 1965, NA,
                            "SPSDOGPCOAST", 1938, 1942, 1938,
                            "SSTHORNHPCOAST", NA, 1992, NA,
                            "WROCKPCOAST", 1941, 1969, 1941,
                            "YTROCKNPCOAST", 1940, NA, NA), ncol=4, byrow=T)
trim.years.df <- as.data.frame(trim.years, stringsAsFactors=F)
trim.years <- trim.years.df %>% 
  rename(stockid=V1, year_tb=V2, year_r=V3, year_catch=V4) %>% 
  mutate(year_tb=as.numeric(year_tb),
         year_catch=as.numeric(year_catch),
         year_trim=pmax(year_tb, year_r, year_catch, na.rm=T))
trim.years$stockid[!(trim.years$stockid%in%pred_stocks$stockid)] # must report 0 = confirms stock IDs spelled correctly
trim.stocks <- trim.years$stockid


# Mark usable years
################################################################################

# Mark usable years
# If the stockid is in "trim.years", mark usable years
# If the stockid isn't in "trim.years", all years are usable
pred_ts$use <- "yes"
for(i in 1:length(trim.stocks)){
  stock <- trim.stocks[i]
  trim.year <- trim.years$year_trim[trim.years$stockid==stock]
  pred_ts$use[pred_ts$stockid==stock & pred_ts$year<=trim.year] <- "no"
}
pred_ts$use <- as.factor(pred_ts$use)

# Mark stocks with assumptions at end of time series
pred_ts$use[pred_ts$stockid=="ARFLOUNDPCOAST" & pred_ts$year>2007] <- "no"


# Plot data
################################################################################

# For headers
top.i <- seq(1, nrow(pred_stocks), 6)

# Sort pred stocks
pred_stocks <- arrange(pred_stocks, stockid)

# Plot data and trimming decisions
figname <- "AppendixC_pred_data_and_trimming.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6, 5), mar=c(1.5, 1.0, 2.5, 0.5), mgp=c(2.5,0.5,0), oma=c(3,3,3,3), lwd=0.8)

# Loop through stocks
for(i in 1:nrow(pred_stocks)){
  
  # Subset data
  stock <- pred_stocks$stockid[i]
  sdata <- subset(pred_ts, stockid==stock)
  method <- pred_stocks$method[i]
  print(paste(i, stock))
  
  # Format data
  sdata <- sdata %>% 
    mutate(catch=catch/1000,
           ssb=ssb/1000,
           tb=tb/1000,
           sp=sp/1000,
           r=r/1E6)

  # Year limits
  tmin <- floor(min(sdata$year)/10)*10
  tmax <- ceiling(max(sdata$year)/10)*10 
  
  # Biology limits
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
  plot(tb ~ year, sdata, type="l", bty="n", 
       xlim=c(tmin, tmax), ylim=c(tbmin, tbmax), yaxt="n",
       xlab="", ylab="", las=2, col="blue")
  axis(2, at=c(tbmin, tbmax))
  title(stock, line=0.1, xpd=NA, col.main=ifelse(stock%in%to.cut, "red", "black"))
  if(i %in% top.i){title("Biomass (1000s mt)\ntime series", col.main="blue", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years$year_tb[trim.years$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(tbmin,tbmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=tbmin+(tbmax-tbmin)*0.10, pos=2, labels=trim.yr, col="black", cex=1.2, xpd=NA)
  }
  
  # B. Plot recruitment
  #######################################
  
  # Plot data
  if(sum(!is.na(sdata$r))>0){
    plot(r ~ year, sdata, type="l", bty="n", 
         xlim=c(tmin, tmax), ylim=c(rmin, rmax), yaxt="n",
         xlab="", ylab="", las=2, col="purple")
    axis(2, at=c(rmin, rmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Recruitment (millions)\ntime series", col.main="purple", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years$year_r[trim.years$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(rmin,rmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=rmin+(rmax-rmin)*0.10, pos=2, labels=trim.yr, col="black", cex=1.2, xpd=NA)
  }
  
  # C. Plot catch
  #######################################
  
  # Plot data
  plot(catch ~ year, sdata, type="l", bty="n", 
       xlim=c(tmin, tmax), ylim=c(cmin, cmax), yaxt="n",
       xlab="", ylab="", las=2, col="darkgreen")
  axis(2, at=c(cmin, cmax))
  if(i %in% top.i){title("Catch (1000s mt)\ntime series", col.main="darkgreen", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years$year_catch[trim.years$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(cmin,cmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=cmin+(cmax-cmin)*0.10, pos=2, labels=trim.yr, col="black", cex=1.2, xpd=NA)
  }
  
  # D. Plot surplus production
  #######################################
  
  # Plot SP
  if(sum(!is.na(sdata$sp))>0 & sum(!is.na(sdata$tb))>0){
    
    # Plot data
    plot(sp ~ tb, sdata, type="p", bty="n", 
         xlim=c(tbmin, tbmax), ylim=c(smin, smax), xaxt="n", yaxt="n",
         xlab="", ylab="", col=c("red", "grey60")[as.factor(sdata$use)])
    axis(1, at=c(tbmin, tbmax))
    axis(2, at=c(smin, smax))
    text(x=tbmax, y=smax, pos=2, labels=method, cex=1.1, col="black", xpd=NA)
  
    # Fit and plot SP model
    sdata1 <- subset(sdata, use=="yes")
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
  if(sum(!is.na(sdata$r))>0 & sum(!is.na(sdata$ssb))>0){
    plot(r ~ ssb, sdata, type="p", bty="n", col=c("red", "grey60")[as.factor(sdata$use)],
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


# Trim data
################################################################################

# Trim data
pred_ts1 <- pred_ts %>% 
  # Remove bad stocks
  filter(!stockid%in%to.cut) %>% 
  # Remove years without 
  filter(use=="yes" & !is.na(sp) & !is.na(tb))

# Inspect sample size
pred_n <- pred_ts1 %>% 
  group_by(stockid) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            yrs=paste0(yr1, "-", yr2),
            nyr=n()) %>% 
  arrange(desc(nyr))
stocks.wout.20yr <- subset(pred_n, nyr<20)$stockid
length(stocks.wout.20yr)

# Remove stocks with <20 years
pred_ts2 <- pred_ts1 %>% 
  filter(!stockid%in%stocks.wout.20yr)
length(unique(pred_ts2$stockid))

# Pred stocks to use
pred_to_use <- sort(unique(pred_ts2$stockid))
length(pred_to_use)

# Format predator stocks
pred_stocks1 <- pred_stocks %>%
  filter(stockid %in% pred_to_use) %>% 
  rename(region_orig=region) %>% 
  mutate(region=revalue(region_orig, c("Canada East Coast"="USA/Canada East", 
                                       "Europe non EU"="Europe", 
                                       "European Union"="Europe",
                                       "South America"="Humboldt Current", 
                                       "US East Coast"="USA/Canada East", 
                                       "US Southeast and Gulf"="USA/Canada East", 
                                       "US West Coast"="USA/Canada West",
                                       "US Alaska"="USA/Canada West"))) %>% 
  left_join(pred_n, by="stockid")


# Export data
################################################################################

# Export data
pred_ts <- pred_ts2
pred_stocks <- pred_stocks1
save(pred_ts, pred_stocks,
     file=file.path(datadir, "fish_predator_data.Rdata"))










