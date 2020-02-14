
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

# Read data
load(paste(datadir, "data_final_sst.Rdata", sep="/"))

# Helper functions
source(file.path(codedir, "helper_functions.R"))



# Function to fit model
################################################################################

# For testing
# load(paste(datadir, "data_final_sst.Rdata", sep="/"))
# dataset <- data
# dataset_name <- "primary"
# p <- 1
# covariate <- "prey1_b_sd"
# covariate_name <- "prey1"

# Fit surplus production model
fit_sp <- function(dataset, dataset_name, p, covariate, covariate_name){

  # 1. Format data
  ######################################
  
  # Problem stocks
  problem_stocks <- c("ELETERSDB", "SEALIONSCBpup", # production unrelated to abundance
                      "PERPELPERU614S", "PERBOOPERU614S", # enormous influences
                      "COMGUISHETALL", "HUMPBACKCAOR") # enormous influence SDs
  
  # Remove problem stocks
  data <- dataset %>% 
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
    dyn.unload(paste(tmbdir, dynlib("pella_prey_fixed"), sep="/"))
    file.remove(paste(tmbdir, c("pella_prey_fixed.o", "pella_prey_fixed.dll"), sep="/"))
    compile("pella_prey_fixed.cpp")
  }
  
  # Load TMB code
  dyn.load(dynlib("pella_prey_fixed"))
  
  # Input data and parameter starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 BetaT=rep(0.0, nstocks),
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=data$tb_sd,
                     P_t=data$sp_sd,
                     Prey_t=as.numeric(unlist(data[,covariate])))
  
  # Initialization
  model <- MakeADFun(data=input.data, parameters=params, DLL="pella_prey_fixed")
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
  
  
  # 4. Plot results
  ######################################
  
  # Format output
  results <- format_output(sd, stocks)
  
  # Plot results
  plot_ests(results)
  plot_thetas(results)
  
  
  # 5. Export results
  ######################################

  # Outfile name
  outfile <- paste0(dataset_name, "_pella_", "", format(p, nsmall=2), "p_fixed_", covariate_name, ".Rdata")
  
  # Export model objects
  setwd("~/Dropbox/Chris/Rutgers/projects/forage_fish")
  save(data, stocks, nstocks,
       input.data, params,
       model, output, sd, hess, results,
       file=paste(outputdir, outfile, sep="/"))

}

# Fit models
################################################################################

# Variable parameters
# p = 1.00 (50%), 0.55 (45%), 0.20 (40%), 0.01 (37%)
# covariate = prey1_b_sd,  sst_c_sd, prey_b_sd

# Primary prey dataset
load(file.path(datadir, "data_final_sst.Rdata"))
fit_sp(dataset=data, dataset_name="primary", covariate="prey1_b_sd", covariate_name="prey1", p=0.55) # primary prey
fit_sp(dataset=data, dataset_name="primary", covariate="sst_c_sd", covariate_name="sst", p=0.55) # SST

# Composite prey dataset
load(file.path(datadir, "data_composite_final_sst.Rdata"))
fit_sp(dataset=data, dataset_name="composite", covariate="prey1_b_sd", covariate_name="prey1", p=0.55) # primary prey
fit_sp(dataset=data, dataset_name="composite", covariate="prey_b_sd", covariate_name="cprey", p=0.55) # composite prey
fit_sp(dataset=data, dataset_name="composite", covariate="sst_c_sd", covariate_name="sst", p=0.55) # SST


