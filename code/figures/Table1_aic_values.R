

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

# Merge models
models <- c(primary_sp, composite_sp)

# Model name
model_names <- c("Primary-PT-50%", "Primary-PT-45%", "Primary-PT-40%", "Primary-PT-37%",
                 "Composite-PT-50%", "Composite-PT-45%", "Composite-PT-40%", "Composite-PT-37%")

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


# Other models
################################################################################

# PT prey-linked (fixed effects)
sp_prey_fixed <- "pella_best_fixed_prey1.Rdata"

# PT prey-linked (random effects)
sp_prey_random <- "pella_best_random_prey1.Rdata"

# PT prey-linked (group random effects)
# sp_prey_random_group <- "pella_best_random_group_prey1.Rdata"

# PT SST-linked (fixed effects)
sp_sst_fixed <- "pella_best_fixed_sst.Rdata"

# PT SST-linked (random effects)
sp_sst_random <- "pella_best_random_sst.Rdata"

# PT SST-linked (group random effects)
# sp_sst_random_group <- "pella_best_random_group_sst.Rdata"

# Merge models
models <- c(sp, 
            sp_prey_fixed, sp_prey_random, sp_prey_random_group,
            sp_sst_fixed, sp_sst_random, sp_sst_random_group)

# Model name
model_names <- c("PT-50%", "PT-45%", "PT-40%", "PT-37%", 
                 "PT-45%-Fixed-Prey1", "PT-45%-Random-Prey1", "PT-45%-Random-Group-Prey1", 
                 "PT-45%-Fixed-SST", "PT-45%-Random-SST", "PT-45%-Random-Group-SST")
length(models)==length(model_names)


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
  mutate(dataset=ifelse(grepl("primary", model), "Primary prey", "Composite prey"),
         model_type=ifelse(grepl("random", model), "Random effects", 
                           ifelse(grepl("fixed", model), "Fixed effects", "No covariate")),
         covariate=ifelse(grepl("prey1", model), "Primary prey",
                          ifelse(grepl("cprey", model), "Composite prey",
                                 ifelse(grepl("sst", model), "SST", "No covariate")))) %>% 
  # Arrange
  group_by(dataset) %>% 
  arrange(dataset, aic) %>% 
  # Calculate delta AIC
  mutate(daic=aic-min(aic))
  

# Export table
################################################################################

# Export data
write.csv(aic_final, paste(tabledir, "Table1_model_aic_comparison.csv", sep="/"), row.names=F)

