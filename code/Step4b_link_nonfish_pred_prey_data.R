

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
load(file.path(datadir, "nonfish_predator_data.Rdata"))

# Read pred-to-prey key
key_orig <- read.csv(file.path(tabledir, "nonfish_predator_to_prey_key.csv"), as.is=T)

# Format key
key <- key_orig %>% 
  select(stocklong, prey1_stocks) %>% 
  filter(prey1_stocks!="no overlap") %>% 
  mutate(stockid=revalue(stocklong, c("California sea lion Southern SCB [abundance (total)]"="SEALIONSCBtot",          
                                      "California sea lion Southern SCB [pups]"="SEALIONSCBpup", 
                                      "Elegant tern San Diego Bay [breeding pairs]"="ELETERSDB", 
                                      "Grey seal Sable Islands [pup production]"="GSEALSABLEISL", 
                                      "Humpback whale CA/OR [abundance (total est.)]"="HUMPBACKCAOR", 
                                      "Northern fur seal San Miguel Islands [pups]"="NFURSEALSMI", 
                                      "Pacific harbor seal Coastal Estuaries [abundance (total est.)]"="PHSEALWA", 
                                      "Pacific harbor seal Oregon [abundance (total est.)]"="PHSEALOR", 
                                      "Guanay cormorant 6-14°S (number of adults)"="GUACORPERU614S", 
                                      "Peruvian booby 6-14°S (number of adults)"="PERBOOPERU614S", 
                                      "Peruvian pelican 6-14°S (number of adults)"="PERPELPERU614S", 
                                      "Guanay cormorant Peru (number of adults)"="GUACORPERU", 
                                      "Peruvian booby Peru (number of adults)"="PERBOOPERU", 
                                      "Peruvian pelican Peru (number of adults)"="PERPELPERU", 
                                      "Guanay cormorant Peruvian Sea (number of individuals)"="GUACORPERUSEA", 
                                      "Peruvian booby Peruvian Sea (number of individuals)"="PERBOOPERUSEA", 
                                      "Peruvian pelican Peruvian Sea (number of individuals)"="PERPELPERUSEA"))) %>% 
  select(stockid, stocklong, prey1_stocks)

# Any duplicated stockids?
anyDuplicated(key$stockid)


# Build data
################################################################################

# Format data
data <- bm_pred_ts %>%
  # Reduce to predator stocks w/ overlapping prey stock
  filter(stocklong %in% key$stocklong) %>% 
  # Add stockid and primary prey stocks
  left_join(key, by="stocklong") %>% 
  # Add primary prey
  left_join(select(bm_pred_stocks, stocklong, prey1), by="stocklong") %>% 
  # Add prey time series
  left_join(select(prey_ts, stockid, year, biomass, biomass_type, biomass_units), 
            by=c("prey1_stocks"="stockid", "year")) %>% 
  # Rename prey columns
  rename(prey1_b=biomass, prey1_btype=biomass_type, prey1_bunits=biomass_units) %>% 
  # Filter data
  filter(!is.na(n) & !is.na(prey1_b) & !is.na(sp)) %>% 
  # Standardize data
  group_by(stockid) %>% 
  mutate(n_sd=n/max(n),
         sp_sd=sp/max(n),
         # prey1_b_sd=prey1_b/max(prey1_b), # scale to maximum
         prey1_b_sd=scale(prey1_b)) %>% # scale using z-score
  # Reduce columns
  select(stockid, stocklong, year, catch, n, n_units, sp, sp_units, 
         prey1, prey1_stocks, prey1_b, prey1_btype, prey1_bunits,
         n_sd, sp_sd, prey1_b_sd) 

# Confirm standardizations
data.frame(tapply(data$n_sd, data$stockid, max, na.rm=T)) # must be 1
# data.frame(tapply(data$prey1_b_sd, data$stockid, max, na.rm=T)) # must be 1
data.frame(tapply(data$prey1_b_sd, data$stockid, mean, na.rm=T)) # must be 1

# Convince yourself that SP ~ TB is really the same as SP(sd) ~ TB(sd)
stock <- unique(data$stockid)[4]
par(mfrow=c(1,2))
plot(sp ~ n, data, subset=stockid==stock, bty="n", las=1)
plot(sp_sd ~ n_sd, data, subset=stockid==stock, bty="n", las=1)

# Complete?
# Should all be zero
complete(data)

# Do they all still have >=20 years of data?
# Are there any missing years
data_n <- data %>%
  group_by(stocklong, stockid) %>% 
  summarize(n=n(),
            nyr_missing=n!=length(min(year):max(year))) %>% 
  arrange(n)
if(any(data_n$n < 20)){"WARNING: Some stocks have <20 years of data"}
stocks_use <- data_n$stockid[data_n$n>=20]

# Remove stocks with too little data
data <- filter(data, stockid %in% stocks_use)

# Pred stocks to use
pred_stocks_use <- bm_pred_stocks %>%
  select(-stockid) %>% 
  left_join(select(data_n, stocklong, stockid), by="stocklong") %>% 
  filter(stockid %in% stocks_use) %>% 
  select(region, stockid, stocklong, everything())

# Inspect the Peruvian ones
peru_pops <- pred_stocks_use$stockid[pred_stocks_use$area=="Peru"]
peru_ts <- data %>% 
  filter(stockid %in% peru_pops)
peru_ts_wide <- dcast(peru_ts, year ~ stockid, value.var="n")
complete(peru_ts_wide)
plot(GUACORPERU ~ GUACORPERUSEA, peru_ts_wide)
plot(PERBOOPERU ~ PERBOOPERUSEA, peru_ts_wide)
plot(PERPELPERU ~ PERPELPERUSEA, peru_ts_wide)

# Remove the Peruvian Sea (# of individuals) ones
# The Peru (# of adults) ones are more complete
peru_pops_to_remove <- c("GUACORPERUSEA", "PERBOOPERUSEA", "PERPELPERUSEA")
pred_stocks_use1 <- filter(pred_stocks_use, !stockid %in% peru_pops_to_remove)
data1 <- filter(data, !stockid %in% peru_pops_to_remove)

# Export data
################################################################################

# Export final data
data <- data1
stocks <- pred_stocks_use1
save(data, stocks,
     file=file.path(datadir, "nonfish_pred_data_final.Rdata"))









