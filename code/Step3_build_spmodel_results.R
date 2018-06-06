
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
inputdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017"
outputdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/output"

# Read model output
load(paste(outputdir, "hilborn_etal_2017_spmodel_prey_link.Rdata", sep="/"))
rm(hess, input.data, model, nstocks, output, params, problem.stocks, sd, stocks)

# Read predator data
stocks <- read.csv(paste(inputdir, "hilborn_etal_2017_predator_stocks.csv", sep="/"), as.is=T)


# Build data
################################################################################

# Merge parameter means/errors into wide dataframe
params.of.interest <- c("B0", "r", "sigmaP", "BetaT", "ln_B0", "ln_r", "ln_sigmaP")
results.df.filter <- subset(results.df, param%in%params.of.interest)
beta_avg_wide <- dcast(results.df.filter, stockid ~ param, value.var="estimate")
beta_se_wide <- dcast(results.df.filter, stockid ~ param, value.var="stderror")
beta_wide <- merge(beta_avg_wide, beta_se_wide, by="stockid")
colnames(beta_wide) <- c("stocklong", "b0", "betaT", "ln_b0", "ln_r", "ln_sigmaP", "r", "sigmaP",
                         "b0_se", "betaT_se", "ln_b0_se", "ln_r_se", "ln_sigmaP_se", "r_se", "sigmaP_se")
beta_wide <- select(beta_wide, stocklong, b0, r, betaT, sigmaP, ln_b0, ln_r, ln_sigmaP,
                    b0_se, r_se, betaT_se, sigmaP_se, ln_b0_se, ln_r_se, ln_sigmaP_se)
beta_wide$betaT_min <- beta_wide$betaT - beta_wide$betaT_se
beta_wide$betaT_max <- beta_wide$betaT + beta_wide$betaT_se

# Merge parameter estimates with RAMLDB metadata
final <- stocks %>%
    right_join(beta_wide, by="stocklong")


# Inspect completeness
apply(final, 2, function(x) sum(is.na(x)))


# Export data
################################################################################

# Export data
write.csv(final, paste(outputdir, "hilborn_etal_2017_spmodel_prey_results.csv", sep="/"), row.names=F)



