

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)
library(reshape2)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
load(file.path(datadir, "prey_data.Rdata"))
load(file.path(datadir, "fish_predator_data.Rdata"))

# Read pred-to-prey key
key_orig <- read.csv(file.path(tabledir, "fish_predator_to_prey_key.csv"), as.is=T)

# Format key
key <- key_orig %>% 
  select(stockid, prey_stocks) %>% 
  rename(prey1_stocks=prey_stocks) %>% 
  filter(prey1_stocks!="no overlap")


# Format prey data
################################################################################

# Goal: Create composite prey stock time series and add to prey time series dataframe

# Composite prey stocks
comp_prey <- unique(key$prey1_stocks[grepl(",", key$prey1_stocks)])

# Loop through composite prey stocks
for(i in 1:length(comp_prey)){
  
  # Composite prey stocks
  cprey <- comp_prey[i]
  cprey_stocks <- unlist(strsplit( cprey, ", "))
  
  # Time series
  cprey_ts <- subset(prey_ts, stockid %in% cprey_stocks)
  cprey_ts_wide <- dcast(cprey_ts, year ~ stockid, value.var = "biomass")
  cprey_ts_wide$biomass <- apply(cprey_ts_wide[,2:ncol(cprey_ts_wide)],1,sum)
  cprey_ts_wide$biomass_type <- paste(sort(unique(cprey_ts$biomass_type)), collapse="/")
  cprey_ts_wide$biomass_units <- paste(sort(unique(cprey_ts$biomass_units)), collapse="/")
  
  # Reduce and format
  cprey_ts_use <- cprey_ts_wide %>% 
    filter(!is.na(biomass)) %>%
    mutate(stockid=cprey) %>% 
    select(stockid, year, biomass, biomass_type, biomass_units)
  
  # Merge with others
  if(i==1){cprey_final <- cprey_ts_use}else{cprey_final <- rbind(cprey_final, cprey_ts_use)}
  
}

# Add composite stocks to prey time series
prey_ts <- rbind.fill(prey_ts, cprey_final)



# Build data
################################################################################

# Format data
data <- pred_ts %>%
  # Reduce to predator stocks w/ overlapping prey stock
  filter(stockid %in% key$stockid) %>% 
  # Reduce columns
  select(stockid, stocklong, year, catch, catch_type, catch_units, tb, tb_units, sp, sp_units) %>% 
  # Add primary prey
  left_join(select(pred_stocks, stockid, prey1), by="stockid") %>% 
  # Add overlapping primary prey stocks
  left_join(key, by="stockid") %>% 
  # Add prey time series
  left_join(select(prey_ts, stockid, year, biomass, biomass_type, biomass_units), 
            by=c("prey1_stocks"="stockid", "year")) %>% 
  # Rename prey columns
  rename(prey1_b=biomass, prey1_btype=biomass_type, prey1_bunits=biomass_units) %>% 
  # Filter data
  filter(!is.na(prey1_b)) %>% 
  # Standardie data
  group_by(stockid) %>% 
  mutate(tb_sd=tb/max(tb),
         catch_sd=catch/max(tb), 
         sp_sd=sp/max(tb),
         # prey1_b_sd=prey1_b/max(prey1_b), # standardize to max
         # prey1_b_sd1=(prey1_b-mean(prey1_b))/sd(prey1_b), # manual z-score
         prey1_b_sd=scale(prey1_b)) # center/scale using z-score

# Confirm standardizations
data.frame(tapply(data$tb_sd, data$stockid, max, na.rm=T)) # must be 1
# data.frame(tapply(data$prey1_b_sd, data$stockid, max, na.rm=T)) # must be 1
data.frame(tapply(data$prey1_b_sd, data$stockid, mean, na.rm=T)) # must be 1

# Convince yourself that SP ~ TB is really the same as SP(sd) ~ TB(sd)
stock <- unique(data$stockid)[4]
par(mfrow=c(1,2))
plot(sp ~ tb, data, subset=stockid==stock, bty="n", las=1)
plot(sp_sd ~ tb_sd, data, subset=stockid==stock, bty="n", las=1)

# Complete?
# Should all be zero
complete(data)

# Do they all still have >=20 years of data?
# Are there any missing years
data_n <- data %>%
  group_by(stockid) %>% 
  summarize(n=n(),
            nyr_missing=n!=length(min(year):max(year))) %>% 
  arrange(n)
if(any(data_n$n < 20)){"WARNING: Some stocks have <20 years of data"}
if(any(data_n$nyr_missing==T)){"WARNING: Some stocks are missing a year and SP may be incorrect."}

# Pred stocks
pred_stocks_use <- pred_stocks %>%
  filter(stockid %in% data_n$stockid)


# Export data
################################################################################

# Export final data
stocks <- pred_stocks_use
save(data, stocks,
     file=file.path(datadir, "fish_pred_data_final.Rdata"))









