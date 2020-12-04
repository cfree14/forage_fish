
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/hilborn_etal_2017/"
plotdir <- "figures"

# Read data
taxa <- read.csv(paste(datadir, "hilborn_etal_2017_taxa_info.csv", sep="/"), as.is=T)
prey_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_prey_stocks.csv", sep="/"), as.is=T)
pred_stocks <- read.csv(paste(datadir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)
prey_ts <- read.csv(paste(datadir, "hilborn_etal_2017_prey_time_series.csv", sep="/"), as.is=T)
pred_ts <- read.csv(paste(datadir, "hilborn_etal_2017_predator_time_series.csv", sep="/"), as.is=T)
prey_impt <- read.csv(paste(datadir, "hilborn_etal_2017_prey_importance.csv", sep="/"), as.is=T)
diets <- read.csv(paste(datadir, "hilborn_etal_2017_diet_information.csv", sep="/"), as.is=T)

# Build diet proportion data frame
################################################################################

# Build diet propoportions data frame
# Preferred diet proportions (based on Hilborn et al. 2017)
# prey proportions by mass > by numbers > by energetic contribution > by frequency of occurrence
props <- diets %>% 
  group_by(pred_comm_name, ocean, prey_comm_name) %>% 
  summarize(nrefs=n_distinct(reference),
            refs=paste(sort(unique(reference)), collapse=", "),
            prop_wt=mean(prop_diet_by_wt, na.rm=T)/100,
            prop_n=mean(prop_diet_by_n, na.rm=T)/100,
            prop_energy=mean(prop_diet_by_energy, na.rm=T)/100,
            prop_occur=mean(prop_diet_by_occur, na.rm=T)/100) %>% 
  mutate_all(funs(replace(., is.na(.), NA))) %>% 
  mutate(prop_use=NA,
         prop_use_type=ifelse(!is.na(prop_wt), "wt", 
                              ifelse(!is.na(prop_n), "n", 
                                     ifelse(!is.na(prop_energy), "energy", ifelse(!is.na(prop_occur), "occur", "none")))))

# Add diet proportion to use
props$prop_use[props$prop_use_type=="wt"] <- props$prop_wt[props$prop_use_type=="wt"]
props$prop_use[props$prop_use_type=="n"] <- props$prop_n[props$prop_use_type=="n"]
props$prop_use[props$prop_use_type=="energy"] <- props$prop_energy[props$prop_use_type=="energy"]
props$prop_use[props$prop_use_type=="occur"] <- props$prop_occur[props$prop_use_type=="occur"]

# Inspect proportion source
table(props$prop_use_type)

# Check completeness
apply(props, 2, function(x) sum(is.na(x)))

# Export data
write.csv(props, paste(datadir, "hilborn_etal_2017_diet_proportions.csv", sep="/"), row.names=F)

# Build primary key
################################################################################

# Build primary prey key
prey_key <- props %>% 
  group_by(pred_comm_name, ocean) %>% 
  summarize(nprey=sum(!is.na(prop_use)), 
            prey1=ifelse(nprey>0, prey_comm_name[which.max(prop_use)], "none"),
            prey1_prop=ifelse(is.infinite(max(prop_use, na.rm=T)), NA, max(prop_use, na.rm=T)),
            ff_prop=sum(prop_use, na.rm=T)) %>% 
  mutate(diet_id=paste(pred_comm_name, ocean)) %>% 
  select(diet_id, pred_comm_name, ocean, everything()) %>% 
  filter(nprey>0) %>% 
  group_by()

# Expand predator stock key
pred_stocks <- pred_stocks %>%
  mutate(diet_id=paste(comm_name, ocean)) %>% 
  left_join(select(prey_key, diet_id, nprey, prey1, prey1_prop, ff_prop), by="diet_id")
        

# Build time series data
################################################################################

# Format prey time series
######################################

# Add stockshort
prey_stocks <- prey_stocks %>%
  mutate(stockshort=revalue(stocklong, c("Atlantic herring Northwestern Atlantic Coast"="atl_herring",  
                                         "Atlantic mackerel Gulf of Maine / Cape Hatteras"="atl_mackerel", 
                                         "Atlantic menhaden Atlantic"="atl_menhaden",                   
                                         "California market squid Pacific Coast"="pac_market_squid",       
                                         "Gulf menhaden Gulf of Mexico"="gom_menhaden",             
                                         "Longfin inshore squid Atlantic Coast"="atl_longfin_squid",         
                                         "Northern anchovy Pacific Coast"="pac_anchovy",               
                                         "Pacific chub mackerel Pacific Coast"="pac_mackerel",        
                                         "Pacific hake Pacific Coast"="pac_hake",              
                                         "Pacific sardine Pacific Coast"="pac_sardine",           
                                         "Northern shortfin squid Atlantic Coast"="atl_shortfin_squid")))
prey_ts <- left_join(prey_ts, select(prey_stocks, stocklong, stockshort), by="stocklong")

# Convert to wide format
prey_ts_wide <- dcast(prey_ts, year ~ stockshort, value.var="n")


# Format pred time series
######################################

# Format pred time series
pred_ts1 <- pred_ts %>%
  left_join(select(pred_stocks, stocklong, diet_id, prey1), by="stocklong") %>% 
  rename(prey1_name=prey1) %>% 
  mutate(sp=NA, prey1=NA) %>% 
  left_join(prey_ts_wide, by="year") %>%
  select(-c(stocklong_orig, year_orig)) %>% 
  select(stocklong, reference, year, n, n_units, catch, sp, diet_id, prey1_name, prey1, everything()) 
 
# Add primary prey time series: i <- 1
stocks <- sort(unique(pred_ts1$stocklong))
for(i in 1:length(stocks)){
  stock <- stocks[i]
  prey1 <- pred_stocks$prey1[pred_stocks$stocklong==stock]
  if(!is.na(prey1)){
    prey1_short <- prey_stocks$stockshort[prey_stocks$comm_name==prey1]
    print(paste(i, stock))
    pred_ts1$prey1[pred_ts1$stocklong==stock] <- pred_ts1[pred_ts1$stocklong==stock, prey1_short]
  }
}

# Add surplus production time series: i <- 1; j <- 1
# SP(t) = B(t+1) - B(t) + C(t)
for(i in 1:length(stocks)){

  # Subset data
  stock <- stocks[i]
  sdata <- subset(pred_ts1, stocklong==stock)
  
  # Loop through years
  for(j in 1:nrow(sdata)){
    t <- sdata$year[j]
    t1 <- t+1
    nt <- sdata$n[sdata$year==t]
    nt1 <- sdata$n[sdata$year==t1]
    if(length(nt1)==0){nt1 <- NA}
    ct <- sdata$catch[sdata$year==t]
    ct <- ifelse(is.na(ct), 0, ct)
    if(!is.na(nt) & !is.na(nt1)){
      sp <- nt1 - nt + ct
    }else{
      sp <- NA
    }
    pred_ts1$sp[pred_ts1$stocklong==stock & pred_ts1$year==t] <- sp
  }
  
}


# Subset time series
################################################################################

# Original sample
table(pred_stocks$ocean)

# Candidate stocks
pred_stocks1 <- pred_ts1 %>% 
  group_by(stocklong) %>% 
  summarize(n=sum(!is.na(sp) & !is.na(prey1))) %>% 
  left_join(pred_stocks, by="stocklong") 

# Time series >= 20 
pred_stocks_20yr <- filter(pred_stocks1, n>=20)
table(pred_stocks_20yr$ocean)

# Time series >= 20 & >20% diet is forage fish
pred_stocks_use <- filter(pred_stocks1, n>=20 & ff_prop >=0.2)
table(pred_stocks_use$ocean)

# Subset to candidate stocks and standardize B and SP
pred_ts_use <- pred_ts1 %>% 
  filter(stocklong %in% pred_stocks_use$stocklong & !is.na(sp) & !is.na(prey1)) %>% 
  group_by(stocklong) %>% 
  mutate(n_sd=n/max(n), 
         sp_sd=sp/max(n),
         prey1_sd1=prey1/max(prey1),
         prey1_sd2=prey1-mean(prey1)) %>% 
  select(stocklong, reference, year, n, n_sd, n_units, catch, sp, sp_sd, prey1, prey1_sd1, prey1_sd2, prey1_name)

# Confirm SSB standardization
data.frame(tapply(pred_ts_use$n_sd, pred_ts_use$stocklong, max, na.rm=T)) # must be 1
data.frame(tapply(pred_ts_use$prey1_sd1, pred_ts_use$stocklong, max, na.rm=T)) # must be 1
data.frame(tapply(pred_ts_use$prey1_sd2, pred_ts_use$stocklong, mean, na.rm=T)) # must be 0

# Convince yourself that SP ~ SSB is really the same as SP(sd) ~ SSB(sd)
stock <- unique(pred_ts_use$stocklong)[4]
par(mfrow=c(1,2))
plot(sp ~ n, pred_ts_use, subset=stocklong==stock, bty="n", las=1)
plot(sp_sd ~ n_sd, pred_ts_use, subset=stocklong==stock, bty="n", las=1)

# Inspect completeness
apply(pred_ts_use, 2, function(x) sum(is.na(x)))


# Export data
################################################################################

# Export data
write.csv(pred_ts_use, 
          paste(datadir, "hilborn_etal_2017_time_series_to_use.csv", sep="/"), row.names=F)


