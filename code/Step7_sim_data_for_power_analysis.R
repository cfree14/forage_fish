
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)

# Directories
datadir <- "data"
outputdir <- "output"

# Read data
load(file.path(datadir, "data_composite_final_sst.Rdata"))

# Read model estimates
results <- read.csv(file.path(outputdir, "model_results.csv"), as.is=T) %>% 
  filter(dataset=="composite" & framework=="fixed" & covariate == "composite")


# Helper functions
################################################################################

# Calculate catch
calc_c <- function(b, er){
  c <- b * er
}

# Calculate surplus production
calc_sp <- function(r, K, p, b, prey, theta, sigmaP){
  sp <- (r/p)*b*(1-(b/K)^p) * exp(prey*theta) * exp(rnorm(1, mean=-sigmaP^2/2, sd=sigmaP))
}



# Run simulations
################################################################################

# Parameters
niter <- 100
sigmas <- seq(0, 0.4, 0.1)
thetas <- seq(0, 1, 0.25)
param_grid <- expand.grid(sigma=sigmas, theta=thetas)

# Loop through parameter combinations
x <- 1; y <- 1; z <- 1
data_all <- purrr::map_df(1:nrow(param_grid), function(x) {
  
  # Extract parameters to do
  sigma <- param_grid$sigma[x]
  theta <- param_grid$theta[x]
  
  # Loop through number of iteractions
  data_iter <- purrr::map_df(1:niter, function(y) {
    
    # Loop through stocks
    # data_stock <- purrr::map_df(1, function(z){
    data_stock <- purrr::map_df(1:nrow(results), function(z){
      
      # Stock data
      stock_do <- results$stockid[z]
      sdata <- data %>% 
        ungroup() %>% 
        # Reduce to stock
        filter(stockid==stock_do) %>% 
        # Reduce columns
        select(year, tb, sp, catch, prey_b, prey_b_sd) %>% 
        # Rename columns
        rename(c_obs=catch, b_obs=tb, sp_obs=sp) %>% 
        # Calculate exploitation rate
        mutate(er_obs=c_obs/b_obs) %>% 
        # Reclassify exploitation rate
        mutate(er_obs=ifelse(is.na(er_obs), 0, er_obs),  # ARCTERFOULA
               er_obs=pmin(er_obs, 0.95))
      
      # Stock parameters
      b_max <- max(sdata$b_obs)
      r <- results$r[results$stockid==stock_do]
      K_scaled <- results$k[results$stockid==stock_do]
      K <- K_scaled * b_max
      p <- 0.55
      
      # Add initial year 
      sdata <- sdata %>% 
        # Add columns for simulated biomass
        mutate(b_sim=ifelse(year==min(year), b_obs, NA),
               c_sim=ifelse(year==min(year), calc_c(b_sim, er_obs), NA),
               sp_sim=ifelse(year==min(year), calc_sp(r=r, K=K, p=p, 
                                                      b=b_sim, prey=prey_b_sd, theta=theta, sigma=sigma), NA))
      
      # Loop through years to simulate
      for(i in 2:nrow(sdata)){
        
        # Calculate and record this year's biomass
        b_prev <- sdata$b_sim[i-1]
        sp_prev <- sdata$sp_sim[i-1] 
        c_prev <- sdata$c_sim[i-1] 
        b_now <- b_prev + sp_prev - c_prev
        sdata$b_sim[i] <- b_now 
        
        # Calculate and record this year's catch and production
        er_now <- sdata$er_obs[i]
        prey_now <- sdata$prey_b_sd[i]
        c_now <- calc_c(b=b_now, er=er_now)
        sp_now <- calc_sp(r=r, K=K, p=p, b=b_now, prey=prey_now, theta=theta, sigma=sigma)
        sdata$c_sim[i] <- c_now
        sdata$sp_sim[i] <- sp_now
        
      }
      
      # Format data for export
      sdata_out <- sdata %>%
        mutate(theta=theta, 
               sigma=sigma, 
               iter=y, 
               stockid=stock_do) %>% 
        select(theta, sigma, iter, stockid, everything())
      
      # Return
      return(sdata_out)
      
    })
    
  })
  
  
})


# Plot simulations
################################################################################

# All positive?
freeR::complete(data_all)
problems <- filter(data_all, is.na(b_sim)) # a crazy one: GUACORPERU

# Check sample size
stats <- data_all %>% 
  group_by(theta, sigma, iter) %>% 
  summarize(nstock=n_distinct(stockid)) %>% 
  group_by(theta, sigma) %>%  
  summarize(niter=n_distinct(iter),
            nstocks=mean(nstock))

# Plot and example stock
ex_stock <- stocks$stockid[2]
ex_data <- data_all %>% 
  filter(stockid==ex_stock)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  axis.title.x=element_blank(),
                  plot.title=element_text(size=12),
                  axis.text.x = element_text(angle = 90, vjust = 0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))

# Plot example biomass time series
g <- ggplot(ex_data, aes(x=year, y=b_sim, group=iter)) +
  facet_grid(theta ~ sigma) +
  geom_line(col="grey70") +
  labs(y="Biomass", title=ex_stock) +
  theme_bw() + my_theme
g


# Plot example catch time series
g <- ggplot(ex_data, aes(x=year, y=c_sim, group=iter)) +
  facet_grid(theta ~ sigma) +
  geom_line(col="grey70") +
  labs(y="Catch", title=ex_stock) +
  theme_bw() + my_theme
g

# Plot example production curves
g <- ggplot(ex_data %>% filter(iter==1), 
            aes(x=b_sim/1e3, y=sp_sim/1e3, color=prey_b_sd)) +
  facet_grid(theta ~ sigma, scale="free_y") +
  geom_point() +
  labs(x="Biomass (1000s)", y="Production (1000s)", title=ex_stock) +
  scale_color_gradient2(name="Prey abundance,\nstandardized", 
                        low="darkred", high="navyblue", mid="white", midpoint=0) +
  theme_bw() + my_theme
g

# Export data
saveRDS(data_all, file=file.path(datadir, "sim_data_for_power_analysis.Rds"))










