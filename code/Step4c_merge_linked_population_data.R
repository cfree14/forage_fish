

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

# Read fish predator data
load(file.path(datadir, "fish_pred_data_final.Rdata"))
data1 <- data
stocks1 <- stocks

# Read non-fish predator data
load(file.path(datadir, "nonfish_pred_data_final.Rdata"))
data2 <- data
stocks2 <- stocks

# Merge meta data
################################################################################

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
  rename(area=region_orig, location=areaname, n_units=tb_units) 

# Format 2
stocks2f <- stocks2 %>%
  rename(type=pred_type) %>% 
  select(stockid, stocklong, region, area, location, reference,
         type, class, order, family, species, comm_name,
         n_units, nprey, prey1, prey1_prop, prey_prop)

# Merge data
stocks <- rbind.fill(stocks1f, stocks2f) %>% 
  arrange(stockid)


# Merge time series data
################################################################################

# Format data
colnames(data1)
colnames(data2)
data2 <- rename(data2, tb=n, tb_units=n_units, tb_sd=n_sd)

# Merge data
data <- rbind.fill(data1, data2) %>% 
  arrange(stockid, year)


# Export data
################################################################################

# Export data
save(data, stocks,
     file=file.path(datadir, "data_final.Rdata"))


