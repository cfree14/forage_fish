
# Clear workspace
rm(list = ls())


# Setup
################################################################################

# Packages
library(tidyverse)

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
  #outfile, stockid, param, est , est_lo, est_hi
  
  # Random effects models
  if(grepl("random", x)){
    results1 <- results[[1]] %>% 
      mutate(outfile=x) %>% 
      dplyr::select(outfile, everything())
    
  # Fixed effects models
  }else{
    results1 <- results %>% 
      mutate(outfile=x) %>% 
      # Reshape
      dplyr::select(outfile, everything()) #%>% 
      # gather(key="est_type", value="est", 4:ncol(.)) %>% 
      # # Recode
      # mutate(est_type=gsub("est|_", "", est_type),
      #        param=recode(param, "B0"="k", "BetaT"="betaT"),
      #        param_label=paste(param, est_type, sep="_"), 
      #        param_label=recode(param_label, "r_"="r", "k_"="k", "sigmaP_"="sigmaP", "betaT_"="betaT")) %>% 
      # # Reshape
      # select(-c(param, est_type)) %>% 
      # spread(key="param_label", value="est") %>% 
      # mutate(betaT_inf="none",
      #        betaT_inf=ifelse(betaT_hi<0, "negative", betaT_inf),
      #        betaT_inf=ifelse(betaT_lo>0, "positive", betaT_inf)) %>% 
      # # Rearrange
      # select(outfile, stockid, 
      #        r, r_lo, r_hi,
      #        k, k_lo, k_hi,
      #        betaT, betaT_lo, betaT_hi, betaT_inf,
      #        sigmaP, sigmaP_lo, sigmaP_hi)
  }
  
})

# Inspect
freeR::complete(data1)

# Format merged data
data2 <- data1 %>% 
  mutate(framework=ifelse(grepl("random", outfile), "random", "fixed")) %>% 
  left_join(stocks_prey1 %>% dplyr::select(stockid, type), by="stockid") %>% 
  dplyr::select(outfile, dataset, framework, covariate, type, everything())

# Inspect
freeR::complete(data2)


# Export data
################################################################################

# Export output
write.csv(data2, file=file.path(outputdir, "model_results.csv"), row.names=F)











