

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(RColorBrewer)

# Directories
datadir <- "output"
tabledir <- "tables"


# Which models to compare?
################################################################################

# PT model
p <- c(1.00, 0.55, 0.20, 0.01)
sp <- paste0("pella", format(p, nsmall=2), ".Rdata")

# PT Prey-linked (fixed effecst)
sp_prey_fixed <- "pella_best_fixed_prey1.Rdata"

# Merge models
models <- c(sp, sp_prey_fixed)

# Model name
model_names <- c("PT-50%", "PT-45%", "PT-40%", "PT-37%", 
                 "PT-45%-Fixed-Prey1")
length(models)==length(model_names)


# Build table
################################################################################

# Data frame
aic_df <- data.frame(model=model_names, k=NA, lik=NA, aic=NA, stringsAsFactors=F)

# Loop through models and calculate/record AIC value
for(i in 1:length(models)){
  
  # Load data
  load(paste(datadir, models[i], sep="/"))
  
  # Calculate/record AIC value
  k <- length(output[["par"]])
  lik <- output[["objective"]]
  aic_val <- TMBhelper::TMBAIC(output)
  aic_df$k[i] <- k
  aic_df$lik[i] <- lik
  aic_df$aic[i] <- aic_val
  
}

# Format table
aic_final <- aic_df %>% 
  arrange(aic) %>% 
  mutate(daic=aic-min(aic))
  

# Export table
################################################################################

# Export data
write.csv(aic_final, paste(tabledir, "Table1_model_aic_comparison.csv", sep="/"), row.names=F)

