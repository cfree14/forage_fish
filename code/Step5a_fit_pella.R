
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


# Select data and model
################################################################################

# Remove problem stocks
problem_stocks <- c("ELETERSDB", "SEALIONSCBpup",
                    "PERPELPERU614S", "PERBOOPERU614S", # enormous influences
                    "COMGUISHETALL", "HUMPBACKCAOR") # production unrelated to abundance
data <- data %>% 
  filter(!stockid %in% problem_stocks)

# Parameters
stocks <- unique(data$stockid)
nstocks <- length(stocks)


# Fit production model
################################################################################

# Shape parameters
# 50, 45, 40, 37%
p <- c(1.0, 0.55, 0.20, 0.01)
outfiles <- paste0("pella", format(p, nsmall=2), ".Rdata")

# Loop through shape parameters
for(i in 1:length(p)){

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
                     p=p[i],
                     StockID=as.factor(data$stockid),
                     B_t=data$tb_sd,
                     P_t=data$sp_sd)
  
  # Initialization
  model <- MakeADFun(data=input.data, parameters=params, DLL="pella")
  # model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
  # model$hessian <- F
  # newtonOption(model, smartsearch=TRUE)
  
  # Run model
  output <- TMBhelper::Optimize(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

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

  # Export model objects
  setwd("~/Dropbox/Chris/Rutgers/projects/forage_fish")
  save(data, stocks, nstocks,
       input.data, params,
       model, output, sd, hess, #results,
       file=paste(outputdir, outfiles[i], sep="/"))

}


