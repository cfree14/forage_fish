

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
  # Reduce to predator stock id and columns with prey stocks
  select(stockid, prey1_stocks, prey_add_stocks) %>% 
  # Format prey stock columns
  mutate(prey1_stocks_new=ifelse(prey1_stocks=="no overlap", NA, prey1_stocks),
         prey_add_stocks_new=ifelse(prey_add_stocks%in%c("none", "no overlap"), NA, prey_add_stocks)) %>% 
  # Merge prey stock columns
  mutate(prey_stocks_all=paste(prey1_stocks_new, prey_add_stocks_new, sep=", ") %>% gsub(", NA", "", .) %>% recode("NA"="no overlap")) %>% 
  # Reduce columns
  select(stockid, prey_stocks_all, prey1_stocks_new, prey_add_stocks_new) %>% 
  rename(prey_stocks_primary=prey1_stocks_new, prey_stocks_add=prey_add_stocks_new) %>% 
  # Reclassify a few
  # Stocks with ILLEXNAFO3-4 as primary prey cannot add secondary prey because 	ILLEXNAFO3-4 is in relative units
  mutate(prey_stocks_all=ifelse(prey_stocks_primary=="ILLEXNAFO3-4", "ILLEXNAFO3-4", prey_stocks_all),
         prey_stocks_all=ifelse(prey_stocks_add=="ILLEXNAFO3-4" & !is.na(prey_stocks_add), prey_stocks_primary, prey_stocks_all)) %>% 
  # Reduce to predator stocks with overlapping prey stocks
  filter(prey_stocks_all!="no overlap")


# Format prey data
################################################################################

# Goal: Create composite prey stock time series and add to prey time series dataframe

# Composite prey stocks
comp_prey <- unique(key$prey_stocks_all[grepl(",", key$prey_stocks_all)])

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
  left_join(select(pred_stocks, stockid, prey1, prey_impt), by="stockid") %>% 
  # Add overlapping primary prey stocks
  left_join(select(key, stockid, prey_stocks_all), by="stockid") %>% 
  # Add prey time series
  left_join(select(prey_ts, stockid, year, biomass, biomass_type, biomass_units), 
            by=c("prey_stocks_all"="stockid", "year")) %>% 
  # Rename prey columns
  rename(prey_b=biomass, prey_btype=biomass_type, prey_bunits=biomass_units) %>% 
  # Filter data
  filter(!is.na(prey_b)) %>% 
  # Standardie data
  group_by(stockid) %>% 
  mutate(tb_sd=tb/max(tb),
         catch_sd=catch/max(tb), 
         sp_sd=sp/max(tb),
         # prey1_b_sd=prey1_b/max(prey1_b), # standardize to max
         # prey1_b_sd1=(prey1_b-mean(prey1_b))/sd(prey1_b), # manual z-score
         prey_b_sd=scale(prey_b)) %>%  # center/scale using z-score
  ungroup()

# Confirm standardizations
data.frame(tapply(data$tb_sd, data$stockid, max, na.rm=T)) # must be 1
# data.frame(tapply(data$prey1_b_sd, data$stockid, max, na.rm=T)) # must be 1
data.frame(tapply(data$prey_b_sd, data$stockid, mean, na.rm=T)) # must be 1

# Convince yourself that SP ~ TB is really the same as SP(sd) ~ TB(sd)
stock <- unique(data$stockid)[4]
par(mfrow=c(1,2))
plot(sp ~ tb, data, subset=stockid==stock, bty="n", las=1)
plot(sp_sd ~ tb_sd, data, subset=stockid==stock, bty="n", las=1)

# Complete?
# Should all be zero
complete(data)

# Composite units?
table(data$prey_bunits)
table(data$prey_btype)

# Do they all still have >=20 years of data?
# Are there any missing years
data_n <- data %>%
  group_by(stockid) %>% 
  summarize(n=n(),
            nyr_missing=n!=length(min(year):max(year))) %>% 
  arrange(n)
if(any(data_n$n < 20)){"WARNING: Some stocks have <20 years of data"}

# This warning isn't actually right because I calculate SP when the time series is complete
if(any(data_n$nyr_missing==T)){"WARNING: Some stocks are missing a year and SP may be incorrect."} 

# Pred stocks
pred_stocks_use <- pred_stocks %>%
  filter(stockid %in% data_n$stockid)


# Export data
################################################################################

# Export final data
stocks <- pred_stocks_use
save(data, stocks,
     file=file.path(datadir, "fish_pred_data_final_composite.Rdata"))









