
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
library(freeR)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
tmbdir <- "code/tmb_code"
datadir <- "data"
codedir <- "code"
outputdir <- "output"

# Helper functions
source(file.path(codedir, "helper_functions.R"))


# Function to fit model
################################################################################

# For testing
load(file.path(datadir, "data_composite_final_sst.Rdata"))
dataset <- data
dataset_name <- "composite"
p <- 1
  
# Fit surplus production model
fit_sp <- function(dataset, dataset_name, p){

  # 1. Format data
  ######################################

  # Poblem stocks
  problem_stocks <- c("ELETERSDB", "SEALIONSCBpup",
                      "PERPELPERU614S", "PERBOOPERU614S", # enormous influences
                      "COMGUISHETALL", "HUMPBACKCAOR") # production unrelated to abundance
  
  # Remove problem stocks
  data <- dataset %>% 
    ungroup() %>% 
    filter(!stockid %in% problem_stocks)
  
  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)
  
  # 2. Fit production model
  ######################################
  
  # Compile TMB code
  # Only run once to compile code
  setwd(tmbdir)
  if(FALSE){
    dyn.unload(paste(tmbdir, dynlib("pella"), sep="/"))
    file.remove(paste(tmbdir, c("pella.o", "pella.dll"), sep="/"))
    compile("pella.cpp")
  }
    
  # Load TMB code
  dyn.load(dynlib("pella"))
  
  # Input data and parameter starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=data$tb_sd,
                     P_t=data$sp_sd)
    
  # Initialization
  model <- MakeADFun(data=input.data, parameters=params, DLL="pella")
  # model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
  # model$hessian <- F
  # newtonOption(model, smartsearch=TRUE)
  
  # Run model
  output <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

  # 3. Check fit
  ######################################
  
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
    
  # AIC of model
  TMBhelper::TMBAIC(output)

  # 4. Export model fit
  ######################################
  
  # Outfile name
  outfile <- paste0(dataset_name, "_pella_", "", format(p, nsmall=2), "p.Rdata")
  
  # Export model objects
  setwd("~/Dropbox/Chris/Rutgers/projects/forage_fish")
  save(data, stocks, nstocks,
       input.data, params,
       model, output, sd, hess, #results,
       file=paste(outputdir, outfile, sep="/"))

}


# Fit models
################################################################################

# Primary prey dataset
load(file.path(datadir, "data_final_sst.Rdata"))
fit_sp(dataset=data, dataset_name="primary", p=1) # 50%
fit_sp(dataset=data, dataset_name="primary", p=0.55) # 45%
fit_sp(dataset=data, dataset_name="primary", p=0.20) # 40%
fit_sp(dataset=data, dataset_name="primary", p=0.01) # 37%

# Composite prey dataset
load(file.path(datadir, "data_composite_final_sst.Rdata"))
fit_sp(dataset=data, dataset_name="composite", p=1) # 50%
fit_sp(dataset=data, dataset_name="composite", p=0.55) # 45%
fit_sp(dataset=data, dataset_name="composite", p=0.20) # 40%
fit_sp(dataset=data, dataset_name="composite", p=0.01) # 37%


# Lagged datasets
################################################################################

# Load data
load(file.path(datadir, "data_composite_final_sst.Rdata"))

# Build lag-2 data
data_lag2 <- data %>% 
  # Select columns
  select(stockid, year, 
         tb, sp, sst_c, 
         prey_b,
         prey_lag1_b,
         prey_lag2_b, 
         prey1_b,
         prey1_lag1_b,
         prey1_lag2_b) %>% 
  # Drop NAs
  drop_na() %>% 
  # Standardize variables
  group_by(stockid) %>% 
  mutate(tb_sd=tb/max(tb),
         sp_sd=sp/max(tb),
         # SST
         sst_sd=scale(sst_c),
         # Composite prey
         prey_b_sd=scale(prey_b),
         prey_lag1_b_sd=scale(prey_lag1_b),
         prey_lag2_b_sd=scale(prey_lag2_b), 
         # Primary prey
         prey1_b_sd=scale(prey1_b),
         prey1_lag1_b_sd=scale(prey1_lag1_b),
         prey1_lag2_b_sd=scale(prey1_lag2_b)) %>% 
  ungroup()

fit_sp(dataset=data_lag2, dataset_name="composite lag-2", p=1) # 50%
fit_sp(dataset=data_lag2, dataset_name="composite lag-2", p=0.55) # 45%
fit_sp(dataset=data_lag2, dataset_name="composite lag-2", p=0.20) # 40%
fit_sp(dataset=data_lag2, dataset_name="composite lag-2", p=0.01) # 37%




