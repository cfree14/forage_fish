
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data/merged_data"
datadir1 <- "data/hilborn_etal_2017"
datadir2 <- "data/new_data"
outputdir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read predator data
preds <- read.csv(file.path(datadir, "predator_stocks_final.csv"), as.is=T)
pred_ts <- read.csv(file.path(datadir, "predator_time_series_final.csv"), as.is=T)


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


# Format diet data
################################################################################

# Read diet data
diet_orig <- read.csv(file.path(datadir, "predator_diet_proportions_final.csv"), as.is=T)
taxa <- read.csv(file.path(datadir, "taxanomic_info_final.csv"), as.is=T)

# Add scientific names to diet data
diet <- diet_orig %>%
  # Add predator sci name
  left_join(select(taxa, comm_name, species), by=c("pred_comm"="comm_name")) %>% 
  rename(pred_spp=species) %>% 
  mutate(pred_spp=ifelse(pred_comm=="Pacific bonito", "Sarda chiliensis", pred_spp),
         pred_spp=ifelse(pred_comm=="Common murre", "Uria aalge", pred_spp)) %>% 
  # Add prey sci name
  left_join(select(taxa, comm_name, species), by=c("prey_comm"="comm_name")) %>% 
  rename(prey_spp=species) %>% 
  # Rearrange columns
  select(source, dietid, pred_comm, pred_spp, region, prey_comm, prey_spp, everything()) 

# Diet id key
diet_key <- diet %>%
  filter(prop_use_type!="none") %>% 
  group_by(dietid) %>% 
  summarise(nprey=n(),
            prey1=prey_comm[which.max(prop_use)], 
            prey1_prop=max(prop_use), 
            prey_prop=sum(prop_use))


# Helper functions
################################################################################

# Function to calculate surplus production: 
# SP(t) = TB(t+1) - TB(t) + C(t)
# tb <- subset(bdata, assessid==unique(bdata$assessid)[1])$tb
# catch <- subset(bdata, assessid==unique(bdata$assessid)[1])$catch
calc_sp <- function(tb, catch){
  sp <- c(tb[2:length(tb)] - tb[1:(length(tb)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}


# Identify predator stocks
################################################################################

# Filter predator data
preds1 <- preds %>% 
  filter(pred_type!="fish") %>% 
  filter(n_yr>=15) %>%
  filter(diet_yn=="Y") %>% 
  filter(diet_prop>=0.2)

# Format predator data
preds2 <- preds1 %>% 
  mutate(name=paste0(comm_name, " (", species, ")")) %>% 
  left_join(diet_key, by="dietid")

# Export table
write.csv(preds2, file.path(tabledir, "TableS7_bird_mammal_predator_populations.csv"), row.names=F)


# Build predator time series
################################################################################

# Format predator time series
pred_ts1 <- pred_ts %>%
  filter(stocklong %in% preds2$stocklong) %>% 
  select(-c(catch, notes)) %>% 
  arrange(stocklong, year)

# Add missing years
# 1. Which years are missing?
# 2. Make new rows for those years
pred_ts2 <- pred_ts1
for(i in 1:nrow(preds2)){
  stock <- preds2$stocklong[i]
  sdata <- filter(pred_ts1, stocklong==stock)
  yr1 <- min(sdata$year)
  yr2 <-  max(sdata$year)
  all_years <- yr1:yr2
  available_years <- sdata$year
  missing_yrs <- all_years[!all_years%in%available_years]
  n_missing <- length(missing_yrs)
  if(n_missing>0){
    new_data <- data.frame(source=sdata$source[1],
                           stocklong=sdata$stocklong[1],
                           reference=sdata$reference[1],
                           year=missing_yrs,
                           n=NA,
                           n_units=sdata$n_units[1])
    pred_ts2 <- rbind(pred_ts2, new_data)
    print(paste0(length(missing_yrs), " yrs added to ", stock))
  }
}
pred_ts2 <- arrange(pred_ts2, stocklong, year)

# Calculate surplus production
pred_ts3 <- pred_ts2 %>%
  mutate(catch=0, catch_units=NA) %>% 
  group_by(stocklong) %>%
    mutate(sp=calc_sp(n, catch),
           sp_units=n_units) %>%
    ungroup()

# Erase crazy CA sea lion, Southern SCB (pups) outlier
pred_ts3$n[pred_ts3$stocklong=="California sea lion Southern SCB [abundance (total)]" & pred_ts3$n > 200000] <- NA
table(preds2$pred_type)

# Export data
################################################################################

# Export data
bm_pred_stocks <- preds2
bm_pred_ts <- pred_ts3
save(bm_pred_stocks, bm_pred_ts,
     file=file.path(outputdir, "nonfish_predator_data.Rdata"))


# Visualize data
################################################################################

# Setup figure
figname <- "AppendixD_nonfish_pred_data.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(4, 2), mar=c(1.5, 4.0, 4, 0.5), mgp=c(2.5,1,0), oma=c(5,5,5,5), lwd=0.8)

# Loop through stocks
for(i in 1:nrow(bm_pred_stocks)){
  
  # Subset data
  stock <- bm_pred_stocks$stocklong[i]
  sdata <- bm_pred_ts %>% 
    filter(stocklong==stock) %>% 
    mutate(n=n,
           sp=sp)
  units <- bm_pred_stocks$n_units[i]
  comm_name <- bm_pred_stocks$comm_name[i]
  location <- bm_pred_stocks$location[i]
  print(paste(i, stock))
  
  # Plot abundance
  xmin <- floor1(min(sdata$year), 10)
  xmax <- ceiling1(max(sdata$year), 10)
  nmax <- ceiling(max(sdata$n, na.rm=T))
  plot(n ~ year, sdata, type="b", bty="n", xlab="", ylab=units, pch=16, 
       xlim=c(xmin, xmax), ylim=c(0, nmax), yaxt="n", lwd=1.2,
       cex.axis=1.2, cex.lab=1.2, cex=1.2,
       main=paste(comm_name, location, sep="\n"), cex.main=1.2)
  axis(2, at=c(0, nmax), cex.axis=1.2, las=1)
  
  # Plot surplus production
  smin <- floor(min(sdata$sp, na.rm=T))
  smax <- ceiling(max(sdata$sp, na.rm=T))
  plot(sp ~ n, sdata, bty="n", cex.axis=1.2, cex.lab=1.2, cex=1.2,
       xlim=c(0, nmax), ylim=c(smin, smax), xaxt="n", yaxt="n",
       xlab="Abundance", ylab="Production", pch=16, xpd=NA)
  axis(1, at=c(0, nmax), cex.axis=1.2, las=1)
  axis(2, at=c(smin, smax), cex.axis=1.2, las=1)
  
  # Fit and plot SP model
  sdata1 <- subset(sdata, !is.na(n) & !is.na(sp))
  spfit <- fit.sp.model(sp=sdata1$sp, tb=sdata1$n)
  if(!(inherits(spfit, "try-error"))){
    r <- exp(coef(spfit)["r"])
    k <- exp(coef(spfit)["k"])
    curve(r*x*(1-x/k), from=0, to=nmax, add=T, lty=1, lwd=0.9, col="black")
  }
  
  
}

# Off
dev.off()
graphics.off()




