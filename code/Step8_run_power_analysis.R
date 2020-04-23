
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library(TMB)
library(TMBhelper)
library(tidyverse)
library(freeR)
library(reshape2)

# Directories
tmbdir <- "code/tmb_code"
datadir <- "data"
codedir <- "code"
outputdir <- "output"

# Read data
data_orig <- readRDS(file.path(datadir, "sim_data_for_power_analysis.Rds"))

# Helper functions
source(file.path(codedir, "helper_functions.R"))


# To do list
# Add scenario id
# Add stock id
# Eliminate stocks with negative biomass
# Standardize sim stock biomass and surplus production

# Format data
################################################################################




# Helper function
################################################################################


# Fit surplus production model
fit_sp <- function(dataset, dataset_name, p, covariate, covariate_name){
  
  # 1. Format data
  ######################################
  
  # Parameters
  stocks <- unique(dataset$stockid)
  nstocks <- length(stocks)
  
  # 2. Fit production model
  ######################################
  
  # Compile TMB code
  # Only run once to compile code
  # setwd(tmbdir)
  if(FALSE){
    dyn.unload(paste(tmbdir, dynlib("pella_prey_fixed"), sep="/"))
    file.remove(paste(tmbdir, c("pella_prey_fixed.o", "pella_prey_fixed.dll"), sep="/"))
    compile("pella_prey_fixed.cpp")
  }
  
  # Load TMB code
  dyn.load(file.path(getwd(), tmbdir, dynlib("pella_prey_fixed")))
  
  # Input data and parameter starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 BetaT=rep(0.0, nstocks),
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(dataset),
                     p=p,
                     StockID=as.factor(dataset$stockid),
                     B_t=dataset$tb_sd,
                     P_t=dataset$sp_sd,
                     Prey_t=as.numeric(unlist(dataset[,covariate])))
  
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
  
  # Return results
  return(results)
  
  # # Plot results
  # plot_ests(results)
  # plot_thetas(results)
  
  
  # 5. Export results
  ######################################
  
  # Outfile name
  # outfile <- paste0(dataset_name, "_pella_", "", format(p, nsmall=2), "p_fixed_", covariate_name, ".Rdata")
  
  # Export model objects
  # setwd("~/Dropbox/Chris/Rutgers/projects/forage_fish")
  # save(data, stocks, nstocks,
  #      input.data, params,
  #      model, output, sd, hess, results,
  #      file=paste(outputdir, outfile, sep="/"))
  
}

# Loop through iterations and fit model
################################################################################

# Build scenario and iteration grid
scen_grid <- data_orig %>% 
  select(theta, sigma, iter, stockid) %>% 
  unique() %>% 
  group_by(theta, sigma, iter) %>%
  summarize(nstocks=n()) %>% 
  mutate(scenario_id=paste(theta, sigma, iter, sep="_")) %>% 
  select(scenario_id, everything()) %>% 
  # filter(theta>0.25 & sigma!=0) %>% 
  filter(theta>0 & sigma>0) %>% 
  ungroup()

# Loop through scenario grid
x <- 1
results <-  purrr::map_df(1:nrow(scen_grid), function(x) {
  
  # Reduce to data
  theta_do <- scen_grid$theta[x]
  sigma_do <- scen_grid$sigma[x]
  iter_do <- scen_grid$iter[x]
  scenario_id_do <- scen_grid$scenario_id[x]
  sdata <- data_orig %>% 
    ungroup() %>% 
    filter(theta==theta_do & sigma==sigma_do & iter==iter_do) %>% 
    group_by(stockid) %>% 
    mutate(tb_sd=b_sim/max(b_sim, na.rm=T),
           sp_sd=sp_sim/max(b_sim, na.rm=T)) %>% 
    ungroup()
  
  # Remove problem stocks
  problem_stocks <- sdata %>% 
    group_by(stockid) %>% 
    summarise(problem=any(b_sim <0)) %>% 
    filter(problem)
  sdata <- sdata %>% 
    filter(!stockid%in%problem_stocks$stockid)
  n_distinct(sdata$stockid)
  
  # Try fitting model
  sresults <- try(fit_sp(dataset = sdata, dataset_name = "sim", p=0.55,
                  covariate = "prey_b_sd", covariate_name = "cprey"))
  
  # If it works or doesn't work
  if(!inherits(sresults, "try-error")){
    # Add scenario info
    sresults1 <- sresults %>% 
      mutate(theta=theta_do,
             sigma=sigma_do,
             iter=iter_do,
             scenario_id=scenario_id_do)
  }else{
    sresults1 <- data.frame(theta=theta_do,
                            sigma=sigma_do,
                            iter=iter_do,
                            scenario_id=scenario_id_do)
  }
  
  # Return
  return(sresults1)
  
})

# Export results
saveRDS(results, file=file.path(outputdir, "power_analysis_results.Rds"))

# Calculate proportion
################################################################################

# Calculate proportions
props <- results %>%
  # Only fits with CIs
  filter(param=="BetaT" & !is.na(est_lo)) %>% 
  # Group by scenario and calc stats
  group_by(scenario_id, theta, sigma, iter) %>% 
  summarize(nstocks=n_distinct(stockid), 
            nsigpos=sum(est_lo>0),
            psigpos=nsigpos/nstocks) %>% 
  ungroup() %>% 
  # Calculate means by scenario
  group_by(theta, sigma) %>% 
  summarize(n=n(), 
            psigpos=mean(psigpos)) %>% 
  ungroup()

# Plot power analysis
g <- ggplot(props, aes(x=sigma, y=theta, fill=psigpos)) +
  geom_raster() +
  # Axis
  scale_y_continuous(breaks=seq(0.25,1,0.25)) +
  # Labels
  labs(x="Process variability (σ)", y="Prey influence (θ)") +
  scale_fill_gradientn(name="Proportion of signicant\npositive detections",
                       colors=rev(RColorBrewer::brewer.pal(n=9, "RdBu"))) +
  theme_bw() +
  theme(legend.position = "bottom")
g






