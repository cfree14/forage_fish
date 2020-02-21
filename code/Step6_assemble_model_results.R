
# Clear workspace
rm(list = ls())


# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data"
outputdir <- "output"

# Load primary dataset
load(file.path(datadir, "data_final_sst.Rdata"))
data_prey1 <- data
stocks_prey1 <- stocks

# Load primary dataset
load(file.path(datadir, "data_composite_final_sst.Rdata"))
data_cprey <- data
stocks_cprey <- stocks
rm(data, stocks)

# Are all composite prey stocks in primary prey stocks
n_distinct(data_cprey$stockid) == nrow(stocks_cprey)
n_distinct(data_prey1$stockid) == nrow(stocks_prey1)
sum(!stocks_cprey$stockid %in% stocks_prey1$stockid) # all composite prey stocks are in primary prey


# Build data
################################################################################

# Output files
outfiles <- list.files(outputdir, pattern=".Rdata") %>% .[grepl("fixed|random", .)]

# Loop through and merge
x <- outfiles[1]
data1 <- purrr::map_df(outfiles, function(x) {
  
  # Load data
  load(file.path(outputdir, x))
  
  # Format results based on type
  if(grepl("random", x)){
    results1 <- results[[1]] %>% 
      mutate(outfile=x) %>% 
      dplyr::select(outfile, everything())
  }else{
    results1 <- results %>% 
      mutate(outfile=x) %>% 
      dplyr::select(outfile, everything())
  }
  
})

# Format merged data
data2 <- data1 %>% 
  mutate(dataset=ifelse(grepl("primary", outfile), "primary", "composite"), 
         framework=ifelse(grepl("random", outfile), "random", "fixed"),
         covariate=ifelse(grepl("sst", outfile), "sst",
                          ifelse(grepl("cprey", outfile), "composite", "primary"))) %>% 
  left_join(stocks_prey1 %>% dplyr::select(stockid, type), by="stockid") %>% 
  dplyr::select(outfile, dataset, framework, covariate, type, everything())


# Inspect
freeR::complete(data2)


# Export data
################################################################################

# Export output
write.csv(data2, file=file.path(outputdir, "model_results.csv"), row.names=F)











