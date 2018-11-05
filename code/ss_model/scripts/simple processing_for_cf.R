#processing functions
#========Main run function======================================================
run_speciesx<-function(run_n,autorun,inputlist,monitor_list,inits=NULL,mutate_params,mcmc_adapt,mcmc_burn,mcmc_iter_postburn,mcmc_thin,mcmc_chains){
  starttime<-Sys.time()
  spx<-inputlist$spx
  data<-inputlist$data
  covariates<-inputlist$covariates
  covariates_worst<-inputlist$covariates_worst
  skipping_covariates<-inputlist$skipping_covariates
  fixed_params<-inputlist$fixed_params
  n_cov=dim(covariates)[2]

  # parameters to monitor
  pars <- monitor_list

  #Create data list
  data_list <-list("y" = log(data[[spx]]/1000000),"initial_population_size" = data[[spx]][1]/1000000,
                   "n_cov"=n_cov, "covariates"=as.matrix(covariates),"covariates_worst"=as.matrix(covariates_worst),"pi"=pi,
                   "skipping_covariates"=as.matrix(skipping_covariates))

  #Add the fixed parameters to the data list
  for(i in 1:length(fixed_params)){
    data_list<-c(data_list,fixed_params[i])
  }
  
  #Initialize the model (does adaptive sampling)
  #jmodel = jags.model("ssmlinear.txt", data_list, n.chains = mcmc_chains,n.adapt = mcmc_burn)
  
  #Sample from the posterior distribution. The output of coda_samples() is an
  #mcmc.list object, which some functions can make use of directly.
  if(autorun){
    fit_spx<-run.jags(model=file.path(codedir, "ssmlinear.txt"), data=data_list, monitor=monitor_list,
                inits = inits, n.chains = mcmc_chains, method="rjparallel",
                mutate=list(my_mutate_function,mutate_params))
                #mutate=list(arctan_transform,'S_avg'))
    
    
  } else{
    fit_spx<-run.jags(model=file.path(codedir, "ssmlinear.txt"), data=data_list, monitor=monitor_list,
          inits=inits, n.chains = mcmc_chains, adapt = mcmc_adapt, burnin=mcmc_burn,
          sample=mcmc_iter_postburn, thin=mcmc_thin, method="rjparallel",
          mutate=list(my_mutate_function,mutate_params))
          #mutate=list(arctan_transform,'S_avg'))
  }
  
  #Name the run
  runname<-paste0(spx,"_",run_n)
  
  endtime<-Sys.time() #at end
  print(endtime - starttime)
  
  #Return the input as well as the output from the run_species function to make
  #sure we have the same values used in the modeling when we process the results
  # return(list(run=runname,spx=spx,data=data,data_list=data_list,inputlist=inputlist,
  #             monitor_list=monitor_list,pars=pars,fit_spx=fit_spx,jmodel=jmodel))
  return(list(run=runname,spx=spx,data=data,data_list=data_list,inputlist=inputlist,
    monitor_list=monitor_list,mutate_params=mutate_params,pars=pars,fit_spx=fit_spx))
} #run_speciesx()


#===========Prepare inputs====================================
prepare_data<-function(spx,fixed_param_list,covariate_list,datadir,use_en_thresh,en_thresh,simulation_options=NULL){
  #==========Read in data==========
  #Read in data (a longer data set with additional data)
  d<-read.csv(file.path(datadir,'data_long.csv'),header=T)
  #Subset the data if desired
  n_cov<-length(covariate_list)
  #Exclude rows where any covariate or bird species count is missing values
  yr<-which(names(d)=="year")
  cols<-c("year",spx,covariate_list)
  ok<-complete.cases(d[,cols])
  d<-d[ok,cols]
  
  #Restrict the years if requested in "simulation_options".
  minyr<-min(d$year)
  maxyr<-max(d$year)
  #browser()
  if(!is.null(simulation_options$startyear)){
    minyr<-simulation_options$startyear
  }
  if(!is.null(simulation_options$endyear)){
    maxyr<-simulation_options$endyear
  }
  d<-d[d$year >= minyr & d$year <= maxyr,]
  
  #Get the year on which BHc is split into two time series, if called for (it's added to the fixed_params below)
  BHc_splityear<-NULL
  if(!is.null(simulation_options$BHc_splityear)){
    BHc_splityear<-which(d$year==simulation_options$BHc_splityear)
  }
  
  #======Get fixed parameters===============
  #Read in fixed parameters, by species
  p<-read.csv(file.path(datadir,"params.csv"),stringsAsFactors=FALSE,header=T)
  
  #Get fixed parameters for species x
  cp<-p[p$species==spx,]
  
  #Add the parameters for this species to the "fixed_params" list
  fixed_params<-list()
  for(i in 1:length(fixed_param_list)){
    pname<-fixed_param_list[i]
    temp<-cp$value[cp$param==pname]
    fixed_params<-c(fixed_params,list(temp))
    names(fixed_params)[i]<-pname
  }
  am<-fixed_params$am
  m_base<-fixed_params$m_base
  Mo<-fixed_params$Mo
  
  #Remove m_base from the fixed_params list since it isn't used in the model directly (to prevent annoying warnings)
  fixed_params<-fixed_params[-which(names(fixed_params)=="m_base")]
  
  #Add two dimensional params to the list
  Y<-nrow(d) #number of years (note: it's already been cut to size, potentially, by startyear and endyear)
  nyears<-Y #a separate index used for the likelihood main loop so we can do a prior-only run
  A<-am + 1 #number of age categories
  fixed_params<-c(fixed_params,list("Y"=Y,"A"=A,"nyears"=nyears))
  
  #Rf_avg<-(1-exp(-m_base)) #recruitment fraction should roughly equal the fraction dying
  #Rf_avg_priormean<- log(Rf_avg) #log scale

  #Add a prior mean for BHp_avg: the average productivity (log scale)
  Bhp_avg<-(Mo/2) #offspring per female = max offspring/adult/2
  BHp_avg_log_priormean<-log(Bhp_avg)
  
  #We use the number dying at the largest observed population size as a
  #rule-of-thumb prior for carrying capacity, because recruits should be able to
  #replace mortality in a stable population.  Log scale.
  BHc_avg<-max(d[[spx]]/1000000) * (1-exp(-m_base)) #temp /2 to lower prior a bit
  
  #Use different prior mean values for periods 1 and 2
  if(!is.null(BHc_splityear)){
    BHc1<-max(d[[spx]][1:BHc_splityear]/1000000) * (1-exp(-m_base))
    BHc2<-max(d[[spx]][(BHc_splityear + 1):Y]/1000000) * (1-exp(-m_base))
    BHc_avg1_priormean<-log(BHc1)
    BHc_avg2_priormean<-log(BHc2)
  }else{
    BHc_avg_log_priormean<-log(BHc_avg)
  }
  
  #WARNING: there are discontinuities at 0,-1, and 1 in the inverse arctan transform
  S_avg<- exp(-m_base) #S_avg is .85 for cormorants, .82 for pelicans
  S_avg_priormean<-tan(pi*S_avg - pi/2) #inverse of the arctan transform

  avg_skipping_rate<-0.10 #change this
  SK_avg_priormean<-tan(pi*avg_skipping_rate - pi/2) #inverse arctan transform
  
  #Add either one or two carrying capacity parameters to the fixed_params list.
  if(!is.null(BHc_splityear)){
    fixed_params<-c(fixed_params,list("BHc_splityear"=BHc_splityear,
      "BHc_avg1_priormean"=BHc_avg1_priormean,"BHc_avg2_priormean"=BHc_avg2_priormean))
  }else{
    fixed_params<-c(fixed_params,list("BHc_avg_log_priormean"=BHc_avg_log_priormean))
  }
  
  fixed_params<-c(fixed_params,list("BHp_avg_log_priormean"=BHp_avg_log_priormean,
    "S_avg_priormean"=S_avg_priormean,"SK_avg_priormean"=SK_avg_priormean))
  
  #==========Prepare covariates======
  #Separate the covariates from the rest of the data for convenience
  covariates <- d[,match(covariate_list,names(d))]
  #browser()
  #Trick here: we calculate the means and standard deviations to be used in
  #standardizing (scaling) the data BEFORE making modifications, so that the
  #modifications to the covariates are in the original scaled units
  covariate_means<-colMeans(covariates)
  covariate_stdevs<-sapply(1:ncol(covariates),function(i)sqrt(var(covariates[,i])))
  
  #Find the anchoveta covariate and multiply it by a multiplier, if required
  aname<-NULL
  if(any(grepl("^anch",names(covariates)))){
    aname<-names(covariates)[which(grepl("^anch",names(covariates)))]
    if(length(aname) > 1){stop("More than one covariate has a name that starts with 'anch'")}
    if(length(simulation_options) > 0 && !is.null(simulation_options$anchoveta_multiplier)){
      covariates[[aname]]<-covariates[[aname]] * simulation_options$anchoveta_multiplier
    }
  }

  #Find the F covariate and multiply it by a multiplier, if required
  fname<-NULL
  if(any(grepl("^f_",names(covariates)))){
    fname<-names(covariates)[which(grepl("^f_",names(covariates)))]
    if(length(fname) > 1){stop("More than one covariate has a name that starts with 'f_'")}
    if(length(simulation_options) > 0 && !is.null(simulation_options$F_multiplier)){
      covariates[[fname]]<-covariates[[fname]] * simulation_options$F_multiplier
    }
  }
  
  #Multiply the El Nino timeseries if required, and then save a copy as "unscaled".  We're
  #going to use it below; i.e., we pretend that the multiplied version is unscaled.
  ename<-NULL
  if(any(grepl("^EN",names(covariates)))){
    ename<-names(covariates)[which(grepl("^EN",names(covariates)))]
    if(length(ename) > 1){stop("More than one skipping covariate has a name that starts with EN")}
    if(length(simulation_options) > 0 && !is.null(simulation_options$EN_multiplier)){
      covariates[[ename]]<-covariates[[ename]] * simulation_options$EN_multiplier
    }
    EN_unscaled<-covariates[[ename]] #if a multiplier has been used, this will be a multiplied value
  }
  
  #Scale the covariates (do this before finding the worst values, or you end up with different scalings)
  covariates <-data.frame(sapply(1:ncol(covariates),function(i)standardize(covariates[i],covariate_means[i],covariate_stdevs[i])))

  #Copy covariates; they are modified for the skipping parameter. We zero out
  #all *scaled* values of the El Nino index of the skipping covariates where the
  #*unscaled* value of the original is below the threshold
  skipping_covariates<-covariates
  if(use_en_thresh==TRUE && !is.null(ename) ) {
    skipping_covariates[[ename]][EN_unscaled < en_thresh] <-0 
  }
  
  #Initialize worst covariates (overwritten below)
  covariates_worst<-covariates

  #Choose the worst values during the juvenile period, if desired.  Looks for
  #"EN" at the start of the covariate name, and if found, picks the maximum value; 
  #else picks the minimum.
  if(use_worst_values==TRUE){
    for(i in 1:ncol(covariates)){
      for(j in 1:nrow(covariates)){
        if(grepl("^EN",names(covariates)[i]) || grepl("^f_",names(covariates)[i])){
          temp[j]<-max(covariates[j:min(j+ (am - 1),nrow(covariates)),i]) #find the worst year (highest El Nino)
        } else{
          temp[j]<-min(covariates[j:min(j+ (am - 1),nrow(covariates)),i]) #find the worst year (lowest other index)
        }
      }#j
      covariates_worst[,i]<-temp
    }#i  
  }#if use_worst_values=TRUE

  #Get all of the function arguments as a list so we can include them in the output
  #It's deeply annoying that this is not easy in R (neither match.call nor other functions do what is needed)
  inputargs<-list(spx=spx,fixed_param_list=fixed_param_list,
      covariate_list=covariate_list,datadir=datadir,
      use_en_thresh=use_en_thresh,en_thresh=en_thresh,simulation_options=simulation_options)
  
  #Append the new elements to the list
  reslist<-c(inputargs,list(data=d,covariates=covariates,covariates_worst=covariates_worst,
                      skipping_covariates=skipping_covariates,fixed_params=fixed_params))
  
  #Return the original arguments plus the results
  return(reslist)
}

#========process_mcmc_results===================================================
#Makes density plots, observation and process error plots, and fitted plot.
process_mcmc_results<-function(res,pars_to_plot,plots=TRUE){
  #Extract input from the result for running the prior-only model
  run_n<-res$run
  inputlist<-res$inputlist
  covariate_list<-inputlist$covariate_list
  monitor_list<-res$monitor_list
  mutate_params<-res$mutate_params
  
  #Make a dataframe version of fit_spx (useful)
  spx<-spx
  fit_spx<-res$fit_spx #a runjags object
  data<-res$data
  pars<-pars_to_plot #gets expanded in next code chunk
  basenames<-pars_to_plot
  data_list<-res$data_list
  #fit_spx_df = do.call(rbind.data.frame, as.mcmc(res$fit_spx)) #create a dataframe

  fit_spx_df<-as.data.frame(as.matrix(as.mcmc(res$fit_spx))) #convenient, eh?
  
  #Get a dataframe of parameter names with alternate names and the alphas, betas etc. expanded
  pars_expanded<-expand_pars(fit_spx_df,pars_to_plot,expandable_params,covariate_list,mutate_params)
  
  if(plots){
    print("Running the model with only priors")
    #Set nyears=0 to knock out sections of the code that contain the likelihood
    saved_nyears<-res$data_list$nyears
    inputlist$fixed_params$nyears<-0
    #don't monitor deviance (because it's in the likelihood section)
    saved_monitor_list<-monitor_list
    #Remove the "deviance, obs_dev,pro_dev" elements of the list 
    #WARNING: this causes a hard R crash if not done!
    monitor_list<-monitor_list[-which(match(monitor_list,c("deviance","pro_dev","obs_dev","sdsta","sdobs","CC","loglik")) > 0)]

    #RUN THE MODEL WITHOUT THE LIKELIHOOD (priors only!)
    tempres<-run_speciesx(run_n,FALSE,inputlist,monitor_list,inits=NULL,mutate_params,100,200,1000,1,1)
    
    #restore the values
    inputlist$fixed_params$nyears<-saved_nyears  
    monitor_list<-saved_monitor_list

    print("Summarizing priors...")
    priorfit<-as.mcmc.list(tempres$fit_spx) #as.mcmc.list plots each chain separately, whereas as.mcmc would lump them
    prior_summary<-summary(priorfit)
    priormeans<-prior_summary$statistics[rownames(prior_summary$statistics) %in% pars_expanded$displaypar,1:2]
    priorquantiles<-prior_summary$quantiles[rownames(prior_summary$statistics) %in% pars_expanded$displaypar,c(1,5)]
    prior_results<-data.frame(priormeans,priorquantiles)
    names(prior_results)[3:4]<-c("q_2.5","q_97.5")
    
    print("Making density plots...")
    pcols<-ifelse(nrow(pars_expanded)<= 6,2,3)
    
    #Make priors a dataframe (this calls the mutate function internally)
    priors<-as.data.frame(as.matrix(as.mcmc(tempres$fit_spx)))
    #Drop priors that are not going to be plotted
    priors<-priors[,names(priors) %in% pars_expanded$displaypar]
    #Melt them and add a title    
    priors_long<- melt(priors)
    priors_long$type<-"prior"
        
    #Drop any variables from the posteriors that aren't in the priors, refactor 
    #the variable column to avoid extra levels, melt and add a "type" column
    posteriors<-fit_spx_df[,names(fit_spx_df) %in% names(priors)]
    posteriors_long<-melt(posteriors)
    posteriors_long$type<-"posterior"
    
    #Join the priors and posterior into a long data set
    thedata<-rbind(priors_long,posteriors_long)
    
    #Make the variable name an ordered factor, for display
    thedata$variable<-ordered(thedata$variable, levels=pars_expanded$displaypar,labels=pars_expanded$display_names)
    
    #Plot the data
    priorplot<-ggplot(thedata,aes(x=value, fill=type)) +  
      geom_density(bw="nrd",alpha=.25) + facet_wrap(~ variable,ncol=pcols,scales="free") + 
     ggtitle(res$run) + geom_vline(xintercept = 0,linetype='dotted') 
    # + xlim (-10,10) + ylim(0,1.5) + 
    print(priorplot)
    
    #Get the fitted x[] by year
    print("Summarizing samples...")
    spx_summary<-summary(fit_spx)
    stats<-data.frame(spx_summary)

    # #Get the observation and process error by year
     pdev<-stats[grepl("pro_dev",rownames(stats)),]
     odev<-stats[grepl("obs_dev",rownames(stats)),]
     
    # #Plot mean process and observation error by year 
     #Note to self: Process error = (x[i] - Nm[i])"); Observation error = (y[i] - CC[i])
     pro_dev_plot<-ggplot(pdev,aes(x=data$year,y=pdev$Mean)) + geom_line(linetype=1) + geom_line(aes(x=data$year,y=odev$Mean),linetype=3) + xlab("Year") + 
       ylab("Process error (solid line)\nObservation error (dotted)") + ggtitle(paste(res$run,"process and observation error"))
     print(pro_dev_plot) 

    #Create ribbon plot of the fit
    if("CC" %in% monitor_list){
      print("Plotting fitted data...")
      states = fit_spx_df[,startsWith(names(fit_spx_df), "CC[")]
      df = data.frame("year"=data$year,
                      "mean" = apply(states,2,mean),
                      "low" = apply(states,2,quantile,0.025),
                      "hi" = apply(states,2,quantile,0.975))
      #We plot the ribbon before the path so it comes out underneath
      fitplot <- ggplot(df,aes(x=year,y=mean)) + 
        geom_ribbon(aes(x=year,ymin=df$low,ymax=df$hi), fill="gray",alpha=0.7) +
        geom_path() + 
        geom_point(aes(x=year,data[[spx]]/1000000),colour="red") + 
        ylab("Colony count (millions)") + xlab("Year") + ggtitle(paste(res$run,"fit")) 
      print(fitplot)
       
    } else{
      print("Fitted data not plotted because CC is not in the monitor list")
    }
  }
  
  return (list(fit_spx_df=fit_spx_df,pars_expanded=pars_expanded,prior_results=prior_results))
}



#=========little utility functions==================
#This function is used in runjags to back-transform logged and
#arctan-transformed variables. It takes an MCMC matrix (from a single chain) as
#input, which has dimensions n_variables columns X n_samples rows (not including
#burn-in rows). The other arguments are "varnames": a list of variable names to transform,
#and corresponding lists 1) "transforms": the type of transform to do, and 2) "outnames":
#output names for the transformed variables.
my_mutate_function<-function(mcmc_matrix,mutate_params){
  varnames<-mutate_params$modelpar
  transforms<-mutate_params$transform
  out_names<-mutate_params$displaypar
  outlist<-list()
  for(i in 1:length(varnames)){
    x<-mcmc_matrix[,varnames[i]]
    if(transforms[i]=="arctan"){
      y<-(atan(x) + pi/2)/pi
    } else if(transforms[i]=="log"){
      y<-exp(x)
    }
    outlist<-c(outlist,list(y))
    names(outlist)[length(outlist)]<-out_names[i]
  }
  return(outlist)
} #my_mutate_function()

arctan_transform<-function(x){
  y<-(atan(x) + pi/2)/pi
  return(y)
}

inv_arctan_transform<-function(x){
  y<-tan(pi*x - pi/2)
  return(y)
}

#This is the same as the built-in "scale()" function, except that we can pass it
#a different mean and stdev
standardize<-function(x,meanx,stdevx){
  x<-(x-meanx)/stdevx
  return(x)
}

#======================================================================
#Using a fitted model, find initial values:

get_initial_values<-function(nchains,fit_spx_df){
  
  inits<-list()
  
  for(i in 1:nchains){
    #Take a row at random from the posterior to serve as the initial values
    temp<-fit_spx_df[sample(nrow(fit_spx_df),1,replace=T),]
    
    temp<-temp[,names(temp) %in% pars_expanded$modelpar]

    #Convert sdobs, sdsta to tauobs, tausta for use as input
    names(temp)[names(temp)=="sdobs"]<-"tauobs"
    names(temp)[names(temp)=="sdsta"]<-"tausta"
    temp[,"tauobs"]<-1/(temp[,"tauobs"])^2
    temp[,"tausta"]<-1/(temp[,"tausta"])^2
    
    extended_par_list<-c(expandable_params) #,pro_dev="pro_dev",obs_dev="obs_dev",CC="CC")
    epars<-list() #to hold parameter vectors
    
    #Put initial values into vectors for the parameters that become vectors in the model
    for(j in 1:length(extended_par_list)){
      #Get the basename, extended names and values for expandable parameters
      basename<-extended_par_list[[j]]
      ext_names<-names(temp)[grepl(basename,names(temp))]
      ext_vals<-as.numeric(temp[,match(ext_names,names(temp))])

      #Convert expanded names like alpha[1],alpha[2] into vectors the     
      #First remove the expanded version, then add the vector of values back in with a single name ("alpha")
      temp<-temp[,-match(ext_names,names(temp))]
      epars<-c(epars,list(ext_vals))
      names(epars)[length(epars)]<-basename
    } #for j in 1:length(extended_par_list)
    
    #convert to list and tack the extended params on
    temp<-as.list(temp)
    temp<-c(temp,epars)   
    
    inits<-c(inits,list(temp))
  } #for (i in 1:nchains)
  
  return(inits)  
} #get_initial_values()

#################

#This function takes the monitor list and looks for variables that end in _at
#(arctan transform) or with "_log", and makes a little data frame of the
#variable names actually used inside the model ("modelpar"), the type of
#transform that has been done in the model ("transform"), and a set of names 
#that will used for display ("displaypar"), which by default just strip off the endings.
get_mutate_params<-function(monitor_list){
  arctan_vars<-monitor_list[grepl("_at$",monitor_list)]
  at_names<-gsub("_at$","",arctan_vars)
  log_vars<-monitor_list[grepl("_log$",monitor_list)]
  log_names<-gsub("_log$","",log_vars)
  
  at<-data.frame(modelpar=arctan_vars, displaypar=at_names, transform="arctan",stringsAsFactors = F)
  ln<-data.frame(modelpar=log_vars, displaypar=log_names, transform="log",stringsAsFactors = F)
  mutate_params<-rbind(at,ln)
  return(mutate_params)
}
################################

#Expands the list of parameters; for example expands alpha to alpha[1],
#alpha[2], alpha[3] if there are 3 covariates, associates each with the proper
#covariate and gives them display names.
expand_pars<-function(fit_spx_df,pars_to_plot,expandable_params,covariate_list,mutate_params){
  ncovs<-length(covariate_list)
  pars<-pars_to_plot #gets expanded in next code chunk
  basenames<-pars_to_plot
  data_list<-res$data_list

  #Expand the param list if we have multiple alphas, betas, deltas, etc.
  #Two options: 1) if we provide an MCMC matrix, take the names from there (best option)
  #else 2) guess at how many there are from the number of covariates and the list of expandable params.
  if(!is.null(fit_spx_df)){
    subnames<-NULL
    for(i in 1:length(expandable_params)){
      xnames<-names(fit_spx_df)[grepl(paste0("^",expandable_params[i]),names(fit_spx_df))]
      if(length(xnames) > 1){
        pars<-pars[pars != expandable_params[i]] #remove the parent name (e.g., "alpha")
        subnames<-c(subnames,xnames)
      }
    }
    pars<-c(subnames,pars) #add the sub-names (e.g., "alpha[1]","alpha[2]", etc.) to the head of the list
  }
  
  #create a data frame of the expanded parameters, with transforms for each
  pars_expanded<-as.data.frame(pars,stringsAsFactors = F)
  names(pars_expanded)<-"displaypar"
  pars_expanded$par_order<-1:nrow(pars_expanded) #add a column to keep the order
  
  #Add a column for the model parameters (modelpar)
  tmp<-merge(pars_expanded,mutate_params,by="displaypar",all.x=T)
  #Those parameters without a modelpar name just get the same name as the displaypar
  tmp$modelpar[is.na(tmp$modelpar)]<-tmp$displaypar[is.na(tmp$modelpar)] 
  #Restore the order after the merge
  tmp<-tmp[order(tmp$par_order),]
  pars_expanded<-tmp

  #Add a column for the basename
  pars_expanded$basename<-pars_expanded$displaypar
  pars_expanded$basename<-gsub("\\[.*]","",pars_expanded$basename) #remove all brackets (and contents) from names for the basenames
  
  #Add a column for display names that combine the covariate name (e.g.,
  #anchoveta_SSB) with the list name of the expandables (e.g. "R"), as "R_anchoveta_SSB".
  pars_expanded$display_names<-pars_expanded$displaypar #to start with
  pars_expanded$display_names[pars_expanded$displaypar=="BHc_avg"]<-"R_capacity_mean"
  pars_expanded$display_names[pars_expanded$displaypar=="BHc_avg1"]<-"R_capacity_mean_pre-1965"
  pars_expanded$display_names[pars_expanded$displaypar=="BHc_avg2"]<-"R_capacity_mean_post-1965"
  pars_expanded$display_names[pars_expanded$displaypar=="BHp_avg"]<-"R_productivity_mean"
  pars_expanded$display_names[pars_expanded$displaypar=="SK_avg"]<-"Skipping_mean"
  pars_expanded$display_names[pars_expanded$displaypar=="S_avg"]<-"Survival_mean"
  pars_expanded$display_names[pars_expanded$displaypar=="eta"]<-"R_capacity_vs_anch"
  pars_expanded$display_names[pars_expanded$displaypar=="n0"]<-"Initial pop. size"
  pars_expanded$display_names[pars_expanded$displaypar=="sdsta"]<-"Process StDev"
  pars_expanded$display_names[pars_expanded$displaypar=="sdobs"]<-"Observation StDev"
  
  #Replace alpha,beta,delta by the corresponding covariate name (El Nino, anchoveta, F, etc.)
  for(i in 1:length(expandable_params)){
    #Get a list of alphas, betas or deltas
    greekx<-pars_expanded$displaypar[grepl(expandable_params[i],pars_expanded$displaypar)]
    for(j in 1:length(greekx)){
      idx<-as.numeric(gsub("\\D+","",greekx[j])) #get the number from the parameter name
      covname<-covariate_list[idx] #find the corresponding covariate name
      if(covname=="anchoveta_v2"){covname<-"anchoveta"}
      if(covname=="EN_ICEN"){covname<-"El_Nino"}
      if(covname=="f_walters"){covname<-"F"}
      #Give R,S, and SK better names for display
      prettynames<-expandable_params
      
      #Give them nicer names for plots of covariates vs. something
      for(k in 1:length(expandable_params)){
        if(names(expandable_params)[k]=='R'){names(prettynames)[k]<-"R prod"}
        if(names(expandable_params)[k]=='R1'){names(prettynames)[k]<-"R prod pre-1965"}
        if(names(expandable_params)[k]=='R2'){names(prettynames)[k]<-"R prod post-1965"}
        if(names(expandable_params)[k]=='S'){names(prettynames)[k]<-"Survival"}
        if(names(expandable_params)[k]=='SK'){names(prettynames)[k]<-"Skipping"}
      }
      
      pars_expanded$display_names[match(greekx[j],pars_expanded$displaypar)]<-paste(names(prettynames)[i],"vs",covname)
    }
  }
  
  return(pars_expanded)
}
##################
