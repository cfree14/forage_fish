cat("
  #Declare variables (population matrix, adult mortality, leslie matrix, Dhats, Qs, etc.)
    var N[A,Y],m_ad[Y],M[A,Y],BHpt[Y],BHp[Y],BHct[Y],BHc[Y],Rt[Y],R[Y],St[Y],S[Y],SKt[Y],SK[Y],CC[Y],x[Y],y[Y],Nm[Y],obs_dev[Y],pro_dev[Y];

    model {

    # Prior for initial population size (log scale)
    #NOTE: Artificial loop, for cutting out these lines when calculating priors only
    for(i in 1:min(nyears,1)){
      x[1] <- n0_log #mrun skip
      y[1] ~ dnorm(log(CC[1]),tauobs)
      obs_dev[1] <- y[1] - log(CC[1]);
      pro_dev[1] <- x[1] - log(initial_population_size) #this is a fudge; the first process error is really zero, but is not used
    }    

    #Pre-calculate number of recruits, survival rate, and skipping rate for all years. 
    #This uses a linear model: R = BHp_avg_log + alpha1 * cov1 + alpha2 * cov2 + alpha3 * cov3, etc.
    #Variables that are being searched for in log- and arctan-space end in '_log' and '_at', respectively.
    #Recruitment is based on conditions in year (i-1)..except for year 1, where we lack data
    for(i in 1:nyears){
      #recruitment productivity
      BHpt[i]<- BHp_avg_log +  inprod(covariates_worst[max(i-1,1),],alpha) #log scale
      BHp[i]<-min(exp(BHpt[i]),Mo/2) #Note: we're setting an upper bound on the productivity of Mo/2

      #Recruitment carrying capacity. NOTE: carrying capacity is only influenced by covariate 2, i.e., anchoveta
      #Single-capacity version
      BHct[i]<- BHc_avg_log +  covariates_worst[max(i-1,1),2] * eta #log scale
      BHc[i]<-exp(BHct[i])

      #adult survival
      St[i]<- S_avg_at + inprod(covariates[i,],beta)
      S[i]<-(arctan(St[i]) + pi/2)/pi
      
      #skipping (may use a modified EN index; hence the different covariate set)
      SKt[i]<- SK_avg_at + inprod(skipping_covariates[i,],delta)
      SK[i]<-(arctan(SKt[i]) + pi/2)/pi
    }
    
    #This is an artificial loop: we want to do it one time only, or not at all
    for(i in 1:min(nyears,1)){
      #Create a vector of mortality by age, M.
      m_ad[1]<- -log(S[1]) #Calculate adult mortality rate for year 1
      M[1:am,1]<-rep(0,am)
      M[(am + 1),1]<- m_ad[1] #Set adult mortality (note: index am+1 = age am)
      
      #Set the colony count at n0_log (a random draw based on the observed initial pop 
      #size.  Remember: Nm is not observed)
      CC[1]<-exp(x[1]) 

      #log likelihood only used for model comparison, otherwise ignored
      loglik[1]<-logdensity.norm(y[1], log(CC[1]), tauobs)
      
      #initial true adult population (use SK[1] because no data for year SK[0])
      #if the skipping rate is very high, Nm will be huge.
      Nm[1]<-CC[1]/(1 - SK[1])
  
      #Calculate year 1 recruits
      R[1]=CC[1]/(1/BHp[1] + CC[1]/BHc[1]) #with BHc as vector
  
      #Set the numbers in the N matrix.  This is approximate, since the juvenile
      #age classes should really depend on earlier years
      N[1:am,1]<-rep(R[1],am) #recruits and juveniles
      N[A,1]<-Nm[1] #The number mature (Nm), including error
    }

    for (i in 2:nyears) {
    # State equation
    
    #Create a vector, M, of mortality-by-age for the current year (won't be used til next year)
    m_ad[i]<- -log(S[i]) #get adult mortality from survival adjusted for conditions

    #Set juvenile mortality to zero (subsumed in the BevHolt relation)
    M[1:am,i]<-rep(0,am)
    M[(am + 1),i]<-m_ad[i] #Set adult mortality (note: index am+1 = age am)
    
    #Calculate numbers at age for the current year
    for (j in 2:(A-1)) {
      N[j,i]<-N[j-1,i-1] * exp(-M[j-1,i-1]) #juveniles
    }
    #Keep adults separate (in Nm) for now.
    Nm[i]<-(N[A-1,i-1] * exp(-M[A-1,i-1]) + N[A,i-1] * exp(-M[A,i-1]))

    #Calculate x[i], number mature with lognormal error
    x[i] ~ dnorm(log(Nm[i]),tausta)
    
    #Calculate colony count (mature birds that don't skip breeding)
    CC[i]<-(exp(x[i]) * (1 - SK[i-1]))  

    #Put the number mature (Nm) with error into N.
    N[A,i]<-exp(x[i]) 
    
    #Calculate year i recruits and add them to the population matrix
    R[i]=CC[i]/(1/BHp[i] + CC[i]/BHc[i]) #with BHc as vector

    N[1,i]<-R[i] 
    
    #process error
    pro_dev[i] <- x[i] - log(Nm[i]) 
    
    # Observation error
    y[i] ~ dnorm(log(CC[i]),tauobs) 
    obs_dev[i] <- y[i] - log(CC[i]) 
    } #for i in 2:Y
    
    #This is -2*loglikelihood, with constants dropped (i.e., nlog(sdsta) + nlog(sdobs))
    loglik<- nyears * log(sdsta) + nyears * log(sdobs)

    #Number of params:
    #((alpha + beta + delta) * (number of covariates + intercept)) +
    #  (eta * (1 covariate + intercept)) +
    #  1 (n0) + 2 (SD_obs and SD_process) 
    nparams <- (3 * (n_cov + 1)) + (1 * 2) + 1 + 2
    my_AIC <- (2 * nparams) + loglik
    
    # Priors for covariate coefficients.  Reminder: these are dnorm(mean,tau), not dnorm(mean, sd)
    for(i in 1:n_cov) {
      alpha[i] ~ dnorm(0, .05) #
      beta[i] ~ dnorm(0, .5)
      delta[i] ~ dnorm(0, .5)
    }
    eta ~ dnorm(0,.5) #carrying capacity depends only on anchoveta SSB

    #Priors for average reproduction and carrying capacity
    BHp_avg_log ~ dnorm(BHp_avg_log_priormean,1) #Beverton-Holt productivity (log scale). 
    BHc_avg_log ~ dnorm(BHc_avg_log_priormean,3) #Beverton-Holt capacity (log scale)

    #Priors for average survival 
    S_avg_at ~ dnorm(S_avg_priormean,1) 
    
    #Priors for skipping rate
    SK_avg_at ~ dnorm(SK_avg_priormean,1)

    #standard deviation terms
    tausta ~ dgamma(0.01,0.001) #Eric Ward set these up; they seem to work OK
    sdsta <- 1/sqrt(tausta);
    tauobs ~ dgamma(0.001,0.001);
    sdobs <- 1/sqrt(tauobs);
    
    #Initial population size (x and n0_log are on log scale; initial_population_size is not)
    n0_log ~ dnorm(log(initial_population_size),3)
  }
  ",file="ssmlinear.txt")

#====RUN THE MODEL==================================================
library(rjags)
library(runjags)
library(mcmcplots)
library(ggplot2)
library(MASS)
library(reshape2)
library(grid)
library(gridExtra)

load.module("dic") #load.module is an rjags function

mcmc_adapt = 5000
mcmc_burn = 10000
mcmc_iter_postburn = 10000 #chris, you can set this at 10,000 for most runs
mcmc_thin = 1
mcmc_chains = 3 #and this to 1 if you want to speed things up

#SET THESE TWO DIRECTORIES TO WHEREVER YOU PUT THE DATA(data_long.csv and params.csv) 
#AND SCRIPT(processing functions.R)
data.directory<-paste0(getwd(),"/data/") 
script.directory<-paste0(getwd(),"/scripts/")
source(paste0(script.directory,"simple processing_for_cf.R"))
#output.directory<-getwd()

#Choose the species and run number
species<- c("cormorant", "pelican", "booby")
spx<-species[3] #choose the species to run
run_n<-3 #choose a run number for the model (this is just for tracking)

#Set up the parameters
fixed_param_list<-c("am","m_base","Mo")

#Reminder: alpha=BevHolt productivity; beta=survival; delta=skipping, eta=BevHolt carrying capacity
#This "monitor list" determines which variables get saved by JAGS in each iteration
monitor_list<-c("alpha","beta","delta","eta","BHp_avg_log","BHc_avg_log","S_avg_at","SK_avg_at","n0_log","deviance","obs_dev","pro_dev","CC","sdobs","sdsta")
#These parameters get expanded, depending on the number of covariates.  For example, with 3 covariates we have alpha1, alpha2, and alpha3.
expandable_params<-list(R="alpha",S="beta",SK="delta")
#parameter[1], parameter[2], parameter[3] always correspond to 1=EN, 2=anch, 3=F
covariate_list<-c("EN_ICEN","anchoveta_v2","f_walters") #Other possibilities: "ULOMZ","SST","Harvest.Rate","f_walters","cormorant_dy" (see data).  anchoveta_SSB uses "CarlBetaMonthly" model.
use_worst_values<-TRUE #if true, use the worst conditions for each covariate during the juvenile years
#simulation_options<-list(startyear=1950,endyear=1965) #for shortening the run
simulation_options<-NULL #default is no special options
#If true, allow skipping *only in extreme El Nino years* where the unscaled El
#Nino index exceeds the value "en_thresh".
use_en_thresh<-TRUE #if true, set the El Nino index value to 1 or 0, depending on whether it exceeds a threshold, en_thresh.
en_thresh<-1;

#Prepare the inputs (data, fixed parameters, covariates)
inputlist<-prepare_data(spx,fixed_param_list,covariate_list,data.directory,use_en_thresh,en_thresh,simulation_options)

#Choose initial values as a random row from an existing posterior, or use a single starting point
if((!exists("res") || res$spx != spx) || !exists("pars_expanded")){
  inits<-NULL
  print("Initial values will be set automatically, with the same starting point for each chain")  
} else {
  inits<-get_initial_values(mcmc_chains,fit_spx_df)
  print("Initial values drawn randomly from previous run (fit_spx_df).")
}

#Get list for mutate function (which transforms log- or arctan-transformed results to something comprehensible)
mutate_params<-get_mutate_params(monitor_list)

#RUN THE MODEL and take MCMC samples (ignore the "unused variable Y" warning).  
#Should take 2-3 minutes.  Don't re-size the RStudio plot window while the
#program is running or it will cause a session-ending R crash.
autorun<-TRUE #runs until convergence (you can also set a time limit)
res<-run_speciesx(run_n,autorun,inputlist,monitor_list,inits,mutate_params,mcmc_adapt,mcmc_burn,mcmc_iter_postburn,mcmc_thin,mcmc_chains)

#plus a set of functions for processing
#PROCESS RESULTS
#Make density plots, process and observation error plots, fitted values plots.
pars_to_plot = c("alpha","beta","delta","eta","BHp_avg","BHc_avg","S_avg","SK_avg","n0","sdobs","sdsta")

#NOTE: this runs the model again with priors-only to get values for plots.  
#If the non-parameters aren't removed from the monitor_list it will cause a hard
#session-ending R crash (i.e., don't mess with that part of the code).
#TAKES A MINUTE OR TWO.  Generates a couple of warnings that can be ignored.
processed<-process_mcmc_results(res,pars_to_plot,TRUE)

#Get expanded parameter list and data frame
pars_expanded<-processed$pars_expanded #pars_to_plot, expanded to include each alpha,beta,delta
fit_spx_df<-processed$fit_spx_df

#This is from the mcmcplots library
mcmcfit<-as.mcmc.list(res$fit_spx) #as.mcmc.list plots each chain separately, whereas as.mcmc would lump them
#TAKES A MINUTE OR SO.
mcmc_summary<-summary(mcmcfit)

denplot(mcmcfit,parms=pars_to_plot,plot.title = paste(res$run,"posteriors by chain")) 

#Plot credible intervals for parameters. If "quantiles" argument is missing, the
#default is to use list(outer=c(0.025,0.975),inner=c(0.16,0.84)), which
#corresponds to 95% (thinner line) and 68% (thicker line) credible intervals.
#You want "reorder=FALSE" or it will label the lines incorrectly
caterplot(mcmcfit, parms = pars_expanded$displaypar,labels = pars_expanded$display_names,labels.loc="above",collapse = TRUE,reorder = FALSE)

#DIAGNOSTICS
#This is slow and produces a lot of plots (might be able to subset with parms2plot())
gelman.plot(mcmcfit) 



