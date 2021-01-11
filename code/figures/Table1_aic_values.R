

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


# Which shape parameter is best?
################################################################################

# PT model
p <- c(1.00, 0.55, 0.20, 0.01)
primary_sp <- paste0("primary_pella_", format(p, nsmall=2), "p.Rdata")
composite_sp <- paste0("composite_pella_", format(p, nsmall=2), "p.Rdata")
composite_lag2_sp <- paste0("composite lag-2_pella_", format(p, nsmall=2), "p.Rdata")

# Merge models
models <- c(primary_sp, composite_sp, composite_lag2_sp)

# Model name
model_names <- c("Primary-PT-50%", "Primary-PT-45%", "Primary-PT-40%", "Primary-PT-37%",
                 "Composite-PT-50%", "Composite-PT-45%", "Composite-PT-40%", "Composite-PT-37%",
                 "Composite Lag-2-PT-50%", "Composite Lag-2-PT-45%", "Composite Lag-2-PT-40%", "Composite Lag-2-PT-37%")

# Correct number of names?
length(models)==length(model_names)

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


# Build table
################################################################################

# Models
models <- list.files(datadir, pattern=".Rdata")

# Data frame
aic_df <- data.frame(model=models, k=NA, lik=NA, aic=NA, stringsAsFactors=F)

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
  # Add some helpful columns
  mutate(dataset=ifelse(grepl("primary", model), "Primary prey", 
                        ifelse(grepl("lag", model), "Composite prey lag-2", "Composite prey")),
         model_type=ifelse(grepl("random", model), "Random effects", 
                           ifelse(grepl("fixed", model), "Fixed effects", "No covariate")),
         covariate=ifelse(grepl("prey1", model), "Primary prey",
                          ifelse(grepl("cprey", model), "Composite prey",
                                 ifelse(grepl("sst", model), "SST", "No covariate")))) %>% 
  # Arrange
  select(dataset, covariate, model_type, model, everything()) %>% 
  group_by(dataset) %>% 
  arrange(dataset, aic) %>% 
  # Calculate delta AIC
  mutate(daic=aic-min(aic))
  
  

# Export table
################################################################################

# Export data
write.csv(aic_final, paste(tabledir, "Table1_model_aic_comparison.csv", sep="/"), row.names=F)

