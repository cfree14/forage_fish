

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
tabledir <- "tables"
outputdir <- "output"


# Build our data
################################################################################

# Read our data
stocks1 <- read.csv(file.path(tabledir, "TableS5_fish_predator_stocks_all.csv"), as.is=T) %>% 
  rename(area=areaname, prey_impt_stocks=prey_add_stocks)
stocks2 <- read.csv(file.path(tabledir, "TableS6_nonfish_predator_stocks.csv"), as.is=T) %>% 
  rename(prey1_stocks=prey_stocks_primary, prey_impt_stocks=prey_stocks_add)
stocks2_meta <- read.csv("tables/bird_mammal_predator_populations_metadata.csv", as.is=T) %>% 
  select(stockid, location)
results <- read.csv(file.path(outputdir, "model_results.csv"))

# Build data
data <- bind_rows(stocks1, stocks2) %>% 
  # Add location
  left_join(stocks2_meta) %>% 
  # Add results
  left_join(results %>% filter(outfile=="composite_pella_0.55p_fixed_cprey.Rdata")) %>% 
  filter(!is.na(betaT)) %>% 
  # Common/scientific names
  rename(name_label=name) %>% 
  mutate(comm_name=gsub(" \\(.*", "", name_label),
         sci_name=gsub(".*\\((.*)\\).*", "\\1", name_label)) %>% 
  # Simplify
  select(type, stockid, 
         region, area, location, 
         comm_name, sci_name, name_label, 
         prey1, prey1_prop, prey1_stocks, 
         prey_impt, prey_impt_prop, prey_impt_stocks,
         betaT_inf, betaT, betaT_lo, betaT_lo)

# Inspect
freeR::complete(data)


# Build Sydeman data
################################################################################

# Read Sydeman et al. (2021) populations
pops_syd <- readxl::read_excel("data/sydeman_etal_2021/Sydeman_etal_2021_TableS1.xlsx")
spp_syd <- readxl::read_excel("data/sydeman_etal_2021/Sydeman_etal_2021_TableS2.xlsx")
sites_syd <- readxl::read_excel("data/sydeman_etal_2021/Sydeman_etal_2021_TableS3.xlsx")

# Formtat Sydeman daya
data_syd <- pops_syd %>% 
  # Add species info
  left_join(spp_syd, by="comm_name") %>% 
  # Add site info
  left_join(sites_syd, by="site") %>% 
  # Arrange
  select(ts_id, contributor, 
         ocean_domain, location, site, lat_dd, long_dd, 
         family, comm_name, sci_name, forage_depth, trophic_level, chick_provisioning, 
         duration, n_yrs, slope, everything()) %>% 
  # Remove useless columns
  select(-c(n_timeseries, n_species, n_bird_yrs)) %>% 
  # Format
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         sig=ifelse(grepl("\\*", slope), "*", ""),
         slope=gsub("\\*", "", slope) %>% as.numeric()) 

# Our site key
site_key <- data %>% 
  # mutate(location=ifelse(is.na(location), "", location)) %>% 
  group_by(sci_name) %>% 
  summarize(our_sites=paste(sort(unique(location)), collapse = ", "))

# Inspect overlap
overlap <- data_syd %>% 
  # Scientific names
  filter(sci_name %in% data$sci_name) %>% 
  # Add our sites
  left_join(site_key) %>% 
  # Arrange
  select(ts_id:site, our_sites, everything())




