

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)
library(freeR)
library(reshape2)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"


################################################################################
# MERGE PRIMARY PREY DATA
################################################################################

# Read data
#################################

# Read fish predator data
load(file.path(datadir, "fish_pred_data_final.Rdata"))
data1 <- data %>% ungroup()
stocks1 <- stocks

# Read non-fish predator data
load(file.path(datadir, "nonfish_pred_data_final.Rdata"))
data2 <- data %>% ungroup()
stocks2 <- stocks


# Merge stock meta-data
#################################

# Format data
# stockid, stocklong, region, area, location, 
colnames(stocks1)
colnames(stocks2)

# Format 1
stocks1f <- stocks1 %>%
  mutate(reference="RAM", type="fish") %>% 
  select(stockid, stocklong, region, region_orig, areaname, reference,
         type, class, order, family, species, comm_name,
         tb_units, nprey, prey1, prey1_prop, prey_prop) %>% 
  dplyr::rename(area=region_orig, location=areaname, n_units=tb_units) 

# Format 2
stocks2f <- stocks2 %>%
  dplyr::rename(type=pred_type) %>% 
  select(stockid, stocklong, region, area, location, reference,
         type, class, order, family, species, comm_name,
         n_units, nprey, prey1, prey1_prop, prey_prop)

# Merge data
stocks <- rbind.fill(stocks1f, stocks2f) %>% 
  arrange(stockid)


# Merge time series data
#################################

# Format data
colnames(data1)
colnames(data2)
data2 <- rename(data2, tb=n, tb_units=n_units, tb_sd=n_sd)

# Merge data
data <- rbind.fill(data1, data2) %>% 
  arrange(stockid, year)


# Export data
#################################

# Export data
save(data, stocks,
     file=file.path(datadir, "data_final.Rdata"))

# Rename so its saved when doing composite data below
data_primary <- data
stocks_primary <- stocks



################################################################################
# MERGE COMPOSITE DATA
################################################################################

# Read data
#################################

# Read fish predator data
load(file.path(datadir, "fish_pred_data_final_composite.Rdata"))
data1 <- data %>% ungroup()
stocks1 <- stocks

# Read non-fish predator data
load(file.path(datadir, "nonfish_pred_data_final_composite.Rdata"))
data2 <- data %>% ungroup()
stocks2 <- stocks


# Merge stock meta-data
#################################

# Format data
# stockid, stocklong, region, area, location, 
colnames(stocks1)
colnames(stocks2)

# Format 1
stocks1f <- stocks1 %>%
  mutate(reference="RAM", type="fish") %>% 
  select(stockid, stocklong, region, region_orig, areaname, reference,
         type, class, order, family, species, comm_name,
         tb_units, nprey, prey_prop, prey1, prey1_prop, 
         prey_impt, prey_impt_prop) %>% 
  rename(area=region_orig, location=areaname, n_units=tb_units) 

# Format 2
stocks2f <- stocks2 %>%
  rename(type=pred_type) %>% 
  select(stockid, stocklong, region, area, location, reference,
         type, class, order, family, species, comm_name,
         n_units, nprey, prey_prop, prey1, prey1_prop, 
         prey_impt, prey_impt_prop)

# Merge data
stocks <- rbind.fill(stocks1f, stocks2f) %>% 
  arrange(stockid)

# Inspect
freeR::complete(stocks)


# Format time series data
#################################

# Format data
colnames(data1)
colnames(data2)
data1 <- rename(data1, prey_stocks=prey_stocks_all)
data2 <- rename(data2, tb=n, tb_units=n_units, tb_sd=n_sd)

# Merge data
data <- rbind.fill(data1, data2) %>% 
  arrange(stockid, year) %>% 
  # Add primary prey time series
  left_join(select(data_primary, stockid, year, prey1_stocks, prey1_b, prey1_btype, prey1_bunits, prey1_b_sd), by=c("stockid", "year")) %>% 
  # Arrange columns
  select(stockid, stocklong, year, 
         catch, catch_sd, catch_type, catch_units, 
         tb, tb_sd, tb_units, 
         sp, sp_sd, sp_units, 
         prey1, prey1_stocks, prey1_b, prey1_b_sd, prey1_btype, prey1_bunits, 
         prey_impt, prey_stocks, prey_b, prey_b_sd, prey_btype, prey_bunits, everything())

# Inspect
freeR::complete(data)
table(data$prey_btype)
table(data$prey_bunits)

# Export data
#################################

# Export data
save(data, stocks,
     file=file.path(datadir, "data_composite_final.Rdata"))


