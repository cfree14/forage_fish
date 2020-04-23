

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
  # Reduce to stocki id, primary prey stocks, and additional prey stocks
  select(stocklong, prey1_stocks, prey_add_stocks) %>% 
  # Format prey stock columns
  mutate(prey1_stocks_new=ifelse(prey1_stocks=="no overlap", NA, prey1_stocks),
         prey_add_stocks_new=ifelse(prey_add_stocks%in%c("", "no overlap"), NA, prey_add_stocks)) %>% 
  # Merge prey stock columns
  mutate(prey_stocks_all=paste(prey1_stocks_new, prey_add_stocks_new, sep=", ") %>% 
           gsub(", NA$|NA, ", "", .) %>% recode("NA"="no overlap")) %>% 
  # Reduce columns
  select(stocklong, prey_stocks_all, prey1_stocks_new, prey_add_stocks_new) %>% 
  rename(prey_stocks_primary=prey1_stocks_new, prey_stocks_add=prey_add_stocks_new) %>% 
  # Eliminate predators stocks with no overlapping prey
  filter(prey_stocks_all!="no overlap") %>% 
  # Create stockid
  mutate(stockid=revalue(stocklong, c("Arctic tern Foula (occupied territory)"="ARCTERFOULA",
                                      "Black-legged kittiwake Isle of May (nests, completed)"="BLAKITISLMAY",
                                      "Brandt cormorant South Farallon Island [breeding adults]"="BRACORFIS",
                                      "Brandt cormorant Gulf of Farallon Islands [nests (total)]"="BRACORFIF",
                                      "California sea lion Southern SCB [abundance (total)]"="SEALIONSCBtot",          
                                      "California sea lion Southern SCB [pups]"="SEALIONSCBpup", 
                                      "Common guillemot UU Colony, Farallons Island [population index]"="COMGUIFIUU",
                                      "Common guillemot Southeast Farallons Island [breeding adults]"="COMGUIFISE",
                                      "Common guillemot All Shetland Islands colonies (index relative to 1978)"="COMGUISHETALL",
                                      "Elegant tern San Diego Bay [breeding pairs]"="ELETERSDB", 
                                      "Grey seal Sable Islands [pup production]"="GSEALSABLEISL", 
                                      "Humpback whale CA/OR [abundance (total est.)]"="HUMPBACKCAOR", 
                                      "Northern fur seal San Miguel Islands [pups]"="NFURSEALSMI", 
                                      "Pacific harbor seal Coastal Estuaries [abundance (total est.)]"="PHSEALWA", 
                                      "Pacific harbor seal Oregon [abundance (total est.)]"="PHSEALOR", 
                                      "Pacific harbor seal California [abundance (total est.)]"="PHSEALCA", 
                                      "Pacific harbor seal Eastern Bays [abundance (total est.)]"="PHSEALEBAYS", 
                                      "Pacific harbor seal Juan de la Fuca [abundance (total est.)]"="PHSEALJFUCA", 
                                      "Pacific harbor seal San Juan Islands [abundance (total est.)]"="PHSEALSJI", 
                                      "Guanay cormorant 6-14°S (number of adults)"="GUACORPERU614S", 
                                      "Peruvian booby 6-14°S (number of adults)"="PERBOOPERU614S", 
                                      "Peruvian pelican 6-14°S (number of adults)"="PERPELPERU614S", 
                                      "Guanay cormorant Peru (number of adults)"="GUACORPERU", 
                                      "Peruvian booby Peru (number of adults)"="PERBOOPERU", 
                                      "Peruvian pelican Peru (number of adults)"="PERPELPERU", 
                                      "Guanay cormorant Peruvian Sea (number of individuals)"="GUACORPERUSEA", 
                                      "Peruvian booby Peruvian Sea (number of individuals)"="PERBOOPERUSEA", 
                                      "Peruvian pelican Peruvian Sea (number of individuals)"="PERPELPERUSEA",
                                      "Razorbill UK coast (index relative to 1986)"="RAZBILLUK",
                                      "Pigeon guillemot Farallon (breeding adults)"="PIGGUIFI",
                                      "Common guillemot Farallon (breeding adults)"="COMGUIFI",
                                      #"Black-legged kittiwake Vedoy (occupied nests)"="BLKVEDOY",
                                      "Atlantic puffin Hernyken (apparently occupied burrows)"="APUFFHERNYKEN",
                                      "African penguin W Cape (pairs)"="AFPENWCAPE",
                                      "Cape gannet W Cape (pairs)"="CGANNETWCAPE",
                                      "Cape gannet E Cape (pairs)"="CGANNETECAPE",
                                      #"Rhinoceros auklet Farallon (pairs)"="RAUKFARALLON",
                                      "Northern fur seal St. George"="NFURSTGEORGE",
                                      "Northern fur seal St. Paul"="NFURSTPAUL"))) %>% 
  select(stockid, stocklong, prey_stocks_all, prey_stocks_primary, prey_stocks_add)

# Any duplicated stockids?
anyDuplicated(key$stockid)


# Build composite prey time series
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
data <- bm_pred_ts %>%
  # Reduce to predator stocks w/ overlapping prey stocks
  filter(stocklong %in% key$stocklong) %>% 
  # Add species names for primary prey and important prey
  left_join(select(bm_pred_stocks, stocklong, prey1, prey_impt), by="stocklong") %>% 
  # Add stockid and stockids for ALL prey stocks
  left_join(select(key, stockid, stocklong, prey_stocks_all), by="stocklong") %>% 
  # Add prey time series
  left_join(select(prey_ts, stockid, year, biomass, biomass_type, biomass_units), 
            by=c("prey_stocks_all"="stockid", "year")) %>% 
  # Rename prey columns
  rename(prey_stocks=prey_stocks_all, 
         prey_b=biomass, prey_btype=biomass_type, prey_bunits=biomass_units) %>% 
  # Filter data
  filter(!is.na(n) & !is.na(prey_b) & !is.na(sp)) %>% 
  # Standardize data
  group_by(stockid) %>% 
  mutate(n_sd=n/max(n),
         sp_sd=sp/max(n),
         # prey_b_sd=prey_b/max(prey_b), # scale to maximum
         prey_b_sd=scale(prey_b)) %>% # scale using z-score
  # Reduce columns
  select(stockid, stocklong, year, catch, n, n_units, sp, sp_units, 
         prey1, prey_impt, prey_stocks, prey_b, prey_btype, prey_bunits,
         n_sd, sp_sd, prey_b_sd)

# Confirm standardizations
data.frame(tapply(data$n_sd, data$stockid, max, na.rm=T)) # must be 1
# data.frame(tapply(data$prey1_b_sd, data$stockid, max, na.rm=T)) # must be 1
data.frame(tapply(data$prey_b_sd, data$stockid, mean, na.rm=T)) # must be 0

# Convince yourself that SP ~ TB is really the same as SP(sd) ~ TB(sd)
stock <- unique(data$stockid)[4]
par(mfrow=c(1,2))
plot(sp ~ n, data, subset=stockid==stock, bty="n", las=1)
plot(sp_sd ~ n_sd, data, subset=stockid==stock, bty="n", las=1)

# Complete?
# Should all be zero
complete(data)

# Do they all still have >=15 years of data?
# Are there any missing years
data_n <- data %>%
  group_by(stocklong, stockid) %>% 
  summarize(n=n(),
            nyr_missing=n!=length(min(year):max(year))) %>% 
  arrange(n)
if(any(data_n$n < 15)){"WARNING: Some stocks have <15 years of data"}
stocks_use <- data_n$stockid[data_n$n>=15]

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

# Inspect two Guany cormorants remaining
plot(GUACORPERU ~ GUACORPERU614S, peru_ts_wide) # GUACORPERU is more complete than GUACORPERU614S

# Remove the Peruvian Sea (# of individuals) ones
# The Peru (# of adults) ones are more complete
peru_pops_to_remove <- c("GUACORPERUSEA", "PERBOOPERUSEA", "PERPELPERUSEA", "GUACORPERU614S")
pred_stocks_use1 <- filter(pred_stocks_use, !stockid %in% peru_pops_to_remove)
data1 <- filter(data, !stockid %in% peru_pops_to_remove)

# Inspect SP curve for Guanay cormorant
plot(sp ~ n, data1, subset=stockid=="GUACORPERU")


# Export data
################################################################################

# Export final data
data <- data1
stocks <- pred_stocks_use1
save(data, stocks,
     file=file.path(datadir, "nonfish_pred_data_final_composite.Rdata"))


# Export stocks as CSV for table
stocks_out <- stocks %>% 
  select(region, stockid, name, area, prey1, prey1_prop, prey_impt, prey_impt_prop) %>% 
  left_join(key %>% select(stockid, prey_stocks_primary, prey_stocks_add)) %>% 
  select(region, stockid, name, area, prey1, prey_stocks_primary, prey1_prop, prey_impt, prey_stocks_add, prey_impt_prop) %>% 
  mutate(region=recode(region, 
                       "NE Pacific"="US/Canada West Coast",
                       "NW Atlantic"="US/Canada East Coast",
                       "Humboldt"="Humboldt Current",
                       "NE Pacific"="US/Canada West Coast",
                       "Norwegian Sea"="Europe",
                       "Benguela Current"="South Africa",
                       "Bering Sea"="US/Canada West Coast")) %>% 
  arrange(region, name)


write.csv(stocks_out, file=file.path(tabledir, "nonfish_pred_key_with_stockids.csv"), row.names=F)








