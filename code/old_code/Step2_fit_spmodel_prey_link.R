
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(plyr)
library(dplyr)
library(devtools)
library(reshape2)
devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
tmbdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/code/tmb_code"
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/hilborn_etal_2017"
outputdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/output"
setwd(tmbdir)

# Read data
data <- read.csv(paste(datadir, "hilborn_etal_2017_time_series_to_use.csv", sep="/"), as.is=T)

# Read NLS fits (to id problem stocks)
nlsfits <- read.csv(paste(outputdir, "hilborn_etal_2017_nls_spmodel_fits.csv", sep="/"), as.is=T)
  

# Format data
################################################################################

# Inspect data completeness
apply(data, 2, function(x) sum(is.na(x)))

# Remove problem stocks
sp.perfect <- c("Bigeye tuna Atlantic Ocean", 
                "Swordfish Northern Atlantic",
                "Yellowfin tuna Atlantic Ocean")
sp.nls.fail <- nlsfits$stocklong[is.na(nlsfits$k)]
problem.stocks <- c(sp.perfect)
sum(!problem.stocks %in% sort(unique(data$stocklong))) # must be 0
data <- filter(data, !stocklong%in%problem.stocks)

# Fit production model
################################################################################

# Parameters
stocks <- unique(data$stocklong)
nstocks <- length(stocks)

# Compile TMB code
# Only run once to compile code
if(FALSE){
  dyn.unload(paste(tmbdir, dynlib("spmodel_prey_link"), sep="/"))
  file.remove(paste(tmbdir, c("spmodel_prey_link.o", "spmodel_prey_link.dll"), sep="/"))
  compile("spmodel_prey_link.cpp")
}

# Load TMB code
dyn.load(dynlib("spmodel_prey_link"))

# Input data and parameter starting values
b0_starts <- log(sapply(stocks, function(x) max(data$n_sd[data$stocklong==x])) * 1)
params <- list(ln_B0=b0_starts,
               ln_r=rep(log(0.4), nstocks),
               BetaT=rep(0.2, nstocks),
               ln_sigmaP=rep(log(0.06), nstocks),
               mu_T=0.2,
               ln_sd_T=-5)
input.data <- list(Nstocks=nstocks,
                   Nobs=nrow(data),
                   StockID=as.factor(data$stocklong),
                   B_t=data$n_sd,
                   P_t=data$sp_sd,
                   Prey_t=data$prey1_sd1)

# Initialization
model <- MakeADFun(data=input.data, parameters=params, random="BetaT", DLL="spmodel_prey_link")
# model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
# model$hessian <- F
# newtonOption(model, smartsearch=TRUE)

# Run model
output <- TMBhelper::Optimize(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)


# Diagnose model
################################################################################

# Use hessian to diagnose fixed effects that might cause a problem
hess <- optimHess(par=output$par, fn=model$fn, gr=model$gr)
problem.vals <- which(eigen(hess)$values<0)
if(length(problem.vals)>0 ){
  display <- eigen(hess)$vectors[,problem.vals]
  names(display) = (output$diagnostics$Param)
  cbind(1:length(output$par), output$par, display)
}

# Calculate SD
sd <- try(sdreport(model, hessian.fixed=hess))

# Extract MLE of fixed effects and Empirical Bayes prediction of random effects
ParHat <- model$env$parList()

# Compare sample statistics vs. population statistics for random effects
mean(ParHat$BetaT); output$par['mu_T']
sd(ParHat$BetaT); exp(output$par['ln_sd_T'])

# AIC of model
TMBhelper::TMBAIC(output)


# Format results
################################################################################

# Build parameter estimate dataframe
############################################

# Format parameter estimates
results.mat <- summary.sdreport(sd)
results.df <- data.frame(param=rownames(results.mat),
                         estimate=results.mat[,1],
                         stderror=results.mat[,2])

# Add stockid to parameter estimates
n_params <- table(results.df$param)
global_params <- names(n_params[n_params==1])
stock_params <- names(n_params[n_params==length(stocks)])
results.df$stockid[results.df$param%in%global_params] <- "global"
for(i in 1:length(stock_params)){
  results.df$stockid[results.df$param==stock_params[i]] <- stocks
}
results.df <- subset(results.df, select=c(stockid, param, estimate, stderror))

# Reshape dataframe: long-to-wide
results.wide <- dcast(results.df, stockid ~ param, value.var="estimate", subset=.(stockid!="global"))

# Inspect for bad fits
############################################

# Histogram parameter values
params <- unique(results.df$param)
par(mfrow=c(2,5))
for(i in 1:length(params)){
  vals <- results.df$estimate[results.df$param==params[i]]
  hist(vals, main="", xlab=params[i], ylab="")
}

# Stocks with B0 > 5
results.wide$stockid[results.wide$B0>=5]

# Save results
################################################################################

# Export model objects
save(data, stocks, problem.stocks, nstocks, 
     input.data, params, 
     model, output, sd, hess,
     results.df, results.wide,
     file=paste(outputdir, "hilborn_etal_2017_spmodel_prey_link.Rdata", sep="/"))
