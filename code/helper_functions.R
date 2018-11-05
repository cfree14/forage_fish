

# Format TMB output (fixed effects)
################################################################################

# Format TMB output
format_output <- function(sd, stocks){
  
  # Format parameter estimates
  results.mat <- summary.sdreport(sd)
  results.df <- data.frame(stockid=stocks, 
                           param=rownames(results.mat),
                           estimate=results.mat[,1],
                           stderror=results.mat[,2])
  
  # Reduce to stock-specific parameters
  params_of_interest <- c("B0", "BetaT", "ln_B0", "ln_r", "ln_sigmaP", "r", "sigmaP")
  
  # Estimates, wide
  est_wide <- dcast(results.df, stockid ~ param, value.var="estimate", subset=.(param%in%params_of_interest))
  ste_wide <- dcast(results.df, stockid ~ param, value.var="stderror", subset=.(param%in%params_of_interest))
  
  # Format wide data
  est_wide1 <- est_wide %>% 
    rename(k=B0, betaT=BetaT, ln_k=ln_B0)
  ste_wide1 <- ste_wide %>% 
    rename(k_se=B0, betaT_se=BetaT, ln_k_se=ln_B0, ln_r_se=ln_r, 
           ln_sigmaP_se=ln_sigmaP, r_se=r, sigmaP_se=sigmaP)
  
  # Merge results
  wide <- est_wide1 %>% 
    # Merge estimates and errors
    left_join(ste_wide1, by="stockid") %>% 
    select(stockid, 
           r, r_se,
           ln_r, ln_r_se,
           k, k_se,
           ln_k, ln_k_se,
           betaT, betaT_se,
           sigmaP, sigmaP_se,
           ln_sigmaP, ln_sigmaP_se) %>% 
    # Add confidence intervals
    mutate(ln_r_lo=ln_r-ln_r_se*1.96,
           ln_r_hi=ln_r+ln_r_se*1.96,
           ln_k_lo=ln_k-ln_k_se*1.96,
           ln_k_hi=ln_k+ln_k_se*1.96,
           betaT_lo=betaT-betaT_se*1.96,
           betaT_hi=betaT+betaT_se*1.96,
           betaT_inf="none",
           betaT_inf=ifelse(betaT_lo>0, "positive", betaT_inf),
           betaT_inf=ifelse(betaT_hi<0, "negative", betaT_inf),
           ln_sigmaP_lo=ln_sigmaP-ln_sigmaP_se*1.96,
           ln_sigmaP_hi=ln_sigmaP+ln_sigmaP_se*1.96, 
           r_lo=exp(ln_r_lo),
           r_hi=exp(ln_r_hi),
           k_lo=exp(ln_k_lo),
           k_hi=exp(ln_k_hi),
           sigmaP_lo=exp(ln_sigmaP_lo),
           sigmaP_hi=exp(ln_sigmaP_hi)) %>% 
    # Rearrange
    select(stockid, 
           r, r_se, r_lo, r_hi,
           k, k_se, k_lo, k_hi,
           betaT, betaT_se, betaT_lo, betaT_hi, betaT_inf,
           sigmaP, sigmaP_se, sigmaP_lo, sigmaP_hi)
  
  # Return
  return(wide)
  
}

# Format TMB output (random effects)
################################################################################

# Format TMB output
format_output_re <- function(sd, stocks){
  
  # Format parameter estimates
  results.mat <- summary.sdreport(sd)
  results.df <- data.frame(param=rownames(results.mat),
                           estimate=results.mat[,1],
                           stderror=results.mat[,2])
  
  # Add stockid to parameter estimates
  n_params <- table(results.df$param)
  global_params <- names(n_params[n_params==1])
  group_params <- names(n_params[!n_params%in%c(1, length(stocks))])
  stock_params <- names(n_params[n_params==length(stocks)])
  results.df$stockid[results.df$param%in%global_params] <- "global"
  if(length(group_params)>0){results.df$stockid[results.df$param%in%group_params] <- sort(groups)}
  for(i in 1:length(stock_params)){
    results.df$stockid[results.df$param==stock_params[i]] <- stocks
  }
  results.df <- subset(results.df, select=c(stockid, param, estimate, stderror))
  
  # Format global/group parameters
  #############################################
  
  # Format global parameters
  results_global <- results.df %>% 
    filter(stockid=="global" & param%in%c("mu_T", "sd_T", "sd_group")) %>% 
    select(-stockid)
  
  # Format group parameters
  if(length(group_params)>0){
    results_group <- results.df %>% 
      filter(stockid %in% groups) %>% 
      rename(group=stockid, betaT=estimate, betaT_se=stderror) %>% 
      mutate(betaT_lo=betaT-betaT_se*1.96, 
             betaT_hi=betaT+betaT_se*1.96)
  }
  
  # Format stock specific
  #############################################
  
  # Reduce to stock-specific parameters
  params_of_interest <- c("B0", "BetaT", "ln_B0", "ln_r", "ln_sigmaP", "r", "sigmaP")
  
  # Estimates, wide
  est_wide <- dcast(results.df, stockid ~ param, value.var="estimate", subset=.(param%in%params_of_interest))
  ste_wide <- dcast(results.df, stockid ~ param, value.var="stderror", subset=.(param%in%params_of_interest))
  
  # Format wide data
  est_wide1 <- est_wide %>% 
    rename(k=B0, betaT=BetaT, ln_k=ln_B0)
  ste_wide1 <- ste_wide %>% 
    rename(k_se=B0, betaT_se=BetaT, ln_k_se=ln_B0, ln_r_se=ln_r, 
           ln_sigmaP_se=ln_sigmaP, r_se=r, sigmaP_se=sigmaP)
  
  # Merge results
  results_stock <- est_wide1 %>% 
    # Merge estimates and errors
    left_join(ste_wide1, by="stockid") %>% 
    select(stockid, 
           r, r_se,
           ln_r, ln_r_se,
           k, k_se,
           ln_k, ln_k_se,
           betaT, betaT_se,
           sigmaP, sigmaP_se,
           ln_sigmaP, ln_sigmaP_se) %>% 
    # Add confidence intervals
    mutate(ln_r_lo=ln_r-ln_r_se*1.96,
           ln_r_hi=ln_r+ln_r_se*1.96,
           ln_k_lo=ln_k-ln_k_se*1.96,
           ln_k_hi=ln_k+ln_k_se*1.96,
           betaT_lo=betaT-betaT_se*1.96,
           betaT_hi=betaT+betaT_se*1.96,
           betaT_inf="none",
           betaT_inf=ifelse(betaT_lo>0, "positive", betaT_inf),
           betaT_inf=ifelse(betaT_hi<0, "negative", betaT_inf),
           ln_sigmaP_lo=ln_sigmaP-ln_sigmaP_se*1.96,
           ln_sigmaP_hi=ln_sigmaP+ln_sigmaP_se*1.96, 
           r_lo=exp(ln_r_lo),
           r_hi=exp(ln_r_hi),
           k_lo=exp(ln_k_lo),
           k_hi=exp(ln_k_hi),
           sigmaP_lo=exp(ln_sigmaP_lo),
           sigmaP_hi=exp(ln_sigmaP_hi)) %>% 
    # Rearrange
    select(stockid, 
           r, r_se, r_lo, r_hi,
           k, k_se, k_lo, k_hi,
           betaT, betaT_se, betaT_lo, betaT_hi, betaT_inf,
           sigmaP, sigmaP_se, sigmaP_lo, sigmaP_hi)
  
  # Merge results
  if(length(group_params)>0){
    results <- list(results_stock, results_group, results_global)
  }else{
    results <- list(results_stock, results_global)
  }
  
  # Return
  return(results)
  
}


# Plot model estimates
################################################################################

# Plot model estimates
plot_ests <- function(results){
  par(mfrow=c(2,2))
  pars <- c("r", "k", "betaT", "sigmaP")
  par_labels <- c("r", "K / max(biomass)", expression("θ"["SST"]), expression("σ"["production"]))
  xlim <- matrix(c(0.2,1,
                   0.5,5,
                   0.25,0.5,
                   0.01,0.1), ncol=2, byrow=T)
  for(i in 1:length(pars)){
    vals <- results[, pars[i]]
    xbin <- xlim[i,1]
    xround <- xlim[i,2]
    xmin <- floor(min(vals) / xround) * xround 
    xmax <- ceiling(max(vals) / xround) * xround 
    breaks <- seq(xmin, xmax, xbin)
    hist(vals, xlab=par_labels[i], breaks=breaks, main="", col="grey70", border=F, las=1)
  }
}


# Plot theta estimates
################################################################################

# Plot theta estimates
plot_thetas <- function(results){
  
  # Individual theta estimates
  vals <- results %>% 
    # Add 95% confidence intervals and colors
    mutate(lcolor="grey60",
           pcolor="black",
           lcolor=ifelse(betaT_hi<0, "red", lcolor),
           pcolor=ifelse(betaT_hi<0, "red", pcolor),
           lcolor=ifelse(betaT_lo>0, "blue", lcolor),
           pcolor=ifelse(betaT_lo>0, "blue", pcolor)) %>% 
    arrange(desc(betaT))
  
  # Axis limits
  max_abs <- max(c(abs(min(vals$betaT)), abs(max(vals$betaT))))
  xmax <- freeR::ceiling1(max_abs, 0.5)
  
  # Setup empty plot
  par(mfrow=c(1,1))
  plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=0.8,
       xlim=c(-1*xmax, xmax), ylim=c(1,nrow(vals)), xaxt="n",
       xlab=expression("θ"["Prey"]), ylab="")
  axis(1, at=seq(-1*xmax, xmax, 0.5), cex.axis=0.8)
  
  # Add theta estimates (points) and errors (lines)
  sapply(1:nrow(vals), function(x) lines(x=c(vals$betaT_lo[x], vals$betaT_hi[x]), y=c(x,x), col=vals$lcolor[x]))
  points(vals$betaT, 1:nrow(vals), pch=16, cex=0.8, col=vals$pcolor)
  
  # Add theta=0 line
  lines(x=c(0,0), y=c(1, nrow(vals)), lty=3, col="black", lwd=1.5)
  
  # Add positive/negative influence labels
  n_pos <- sum(vals$betaT_lo>0)
  n_neg <- sum(vals$betaT_hi<0)
  n_neutral <- nrow(vals)-n_pos-n_neg
  text_pos <- paste0(n_pos, " stocks\n", "w/ positive influence")
  text_neg <- paste0(n_neg, " stocks\n", "w/ negative influence")
  # text(x=-1.05, y=1, labels=text_pos, pos=4, adj=1, cex=0.7, col="blue")
  # text(x=1.05, y=nrow(vals)-6, labels=text_neg, pos=2, adj=0, cex=0.7, col="red")
  
  # Print % significant
  perc_sig <- (n_pos+n_neg) / nrow(vals) *100
  print(paste0(round(perc_sig,1), "% of estimates are significant"))
  
}
