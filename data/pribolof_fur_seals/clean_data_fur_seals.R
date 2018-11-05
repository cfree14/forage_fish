

# Clean
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(ggplot2)
library(rio)

# Directories
datadir <- "data/20180730_data/original"

# Read data
data_orig <- import(file.path(datadir, "Northern Fur Seal Diet and Abundance.xlsx"), which=2)
diet_orig <- import(file.path(datadir, "Northern Fur Seal Diet and Abundance.xlsx"), which=1)

# Format abundance data
################################################################################

# Format data
data1 <- data_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(comm_name=`predator common`, species=`predator scientific`, age_class=`age class`) %>% 
  mutate(year=round(year),
         abundance=round(as.numeric(abundance)),
         comm_name=freeR::sentcase(comm_name),
         island=revalue(island, c("st paul"="St. Paul", 
                                  "st george"="St. George")))

# Inspect data
stats <- data1 %>% 
  group_by(island, age_class) %>% 
  summarize(nyr=n(),
            yr_dups=anyDuplicated(year),
            yr1=min(year),
            yr2=max(year),
            yr_check=length(yr1:yr2)==nyr)

# Format data again
data2 <- data1 %>%
  filter(age_class%in%c("idle, class 2 and 5", "harem, class 3")) %>% 
  group_by(comm_name, species, island, year) %>% 
  summarize(abundance=sum(abundance)) %>% 
  mutate(units="# of males (harem males + idle males)")


# Format diet data
################################################################################

# Format diet data
diet <- diet_orig %>% 
  setNames(gsub(" ", "_", tolower(colnames(.)))) %>% 
  rename(pred_comm=predator_common, pred_species=predator_scientific, prey_comm=prey_common, prey_species=prey_scientific, units=units_of_diet, diet_prop=`%_in_diet`) %>% 
  group_by(pred_comm, pred_species, prey_comm, prey_species, island, reference, units) %>% 
  summarize(diet_prop=mean(diet_prop)) %>% 
  ungroup() %>% 
  mutate(pred_comm=sentcase(pred_comm),
         prey_comm=sentcase(prey_comm))


# Format diet data
################################################################################

# Read data
diet_curry <- import(file.path(datadir, "NZ and RSA diet data.xlsx"))



