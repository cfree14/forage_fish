

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "output"
plotdir <- "figures"

# Read data
fe <- read.csv(file.path(datadir, "pella_best_fixed_prey1_results.csv"))
re <- read.csv(file.path(datadir, "pella_best_random_prey1_results.csv"))

# Format data
################################################################################

# Format
combo <- re %>% 
  # Reduce RE columns
  select(stockid, stocklong, type, betaT, betaT_lo, betaT_hi, betaT_inf) %>% 
  rename(betaT_re=betaT, betaT_lo_re=betaT_lo, betaT_hi_re=betaT_hi, betaT_inf_re=betaT_inf) %>% 
  mutate(pcolor_re=revalue(betaT_inf_re, c("negative"="red", "positive"="blue", "none"="black")),
         lcolor_re=revalue(betaT_inf_re, c("negative"="red", "positive"="blue", "none"="grey60"))) %>% 
  # Add FE estimates
  left_join(select(fe, stockid, betaT, betaT_lo, betaT_hi, betaT_inf), by="stockid") %>% 
  rename(betaT_fe=betaT, betaT_lo_fe=betaT_lo, betaT_hi_fe=betaT_hi, betaT_inf_fe=betaT_inf) %>% 
  mutate(pcolor_fe=revalue(betaT_inf_fe, c("negative"="red", "positive"="blue", "none"="black")),
         lcolor_fe=revalue(betaT_inf_fe, c("negative"="red", "positive"="blue", "none"="grey60"))) %>% 
  # Arrange by type a FE estimate
  mutate(type=factor(type, levels=c("mammals", "birds", "fish")),
         lcolor_fe=as.character(lcolor_fe),
         lcolor_re=as.character(lcolor_re),
         tcolor=lcolor_fe,
         tcolor=ifelse(betaT_inf_fe=="none" & betaT_inf_re!="none", lcolor_re, lcolor_fe)) %>% 
  arrange(type, desc(betaT_fe)) %>% 
  # Format stocklong
  mutate(stocklong1=revalue(stocklong, c("African penguin W Cape (pairs)"="African penguin, S Africa",                                 
                                         "Albacore tuna North Atlantic"="Albacore tuna, N Atlantic",                                   
                                         "Albacore tuna North Pacific"="Albacore tuna, N Pacific",                                    
                                         "Atlantic puffin Hernyken (apparently occupied burrows)"="Atlantic puffin, Hernyken",          
                                         "Arctic tern Foula (occupied territory)"="Arctic tern, Foula",                      
                                         "Arrowtooth flounder Pacific Coast"="Arrowtooth flounder, US West Coast",                             
                                         "Black rockfish California"="Black rockfish, California",                                     
                                         "Black-legged kittiwake Vedoy (occupied nests)"="Black-legged kittiwake, Vedoy",                 
                                         "Bluefish Atlantic Coast"="Bluefish, US East Coast",                            
                                         "Brandt cormorant Gulf of Farallon Islands [nests (total)]"="Brandt cormorant, G of Farallon Isl",  
                                         "Brandt cormorant South Farallon Island [breeding adults]"="Brandt cormorant, S Farallon Isl",      
                                         "Cape gannet W Cape (pairs)"="Cape gannet, S Africa",                              
                                         "Chilean jack mackerel Chilean EEZ and offshore"="Chilean jack mackerel, Chile",                
                                         "Pacific chub mackerel Pacific Coast"="Chub mackerel, US West Coast",                         
                                         "Atlantic cod NAFO 3M"="Atlantic cod, NAFO 3M",                                      
                                         "Atlantic cod NAFO 3NO"="Atlantic cod, NAFO 3NO",                                     
                                         "Atlantic cod NAFO 3Pn4RS"="Atlantic cod, NAFO 3Pn4RS",                                 
                                         "Atlantic cod NAFO 3Ps"="Atlantic cod, NAFO 3Ps",                                      
                                         "Atlantic cod NAFO 4TVn"="Atlantic cod, NAFO 4TVn",                                       
                                         "Common guillemot UU Colony, Farallons Island [population index]"="Common guillemot, Farallon Isl",  
                                         "Grey seal Sable Islands [pup production]"="Grey seal, Sable Isl",               
                                         "Guanay cormorant Peru (number of adults)"="Guanay cormorant, Peru",                  
                                         "Hake Northeast Atlantic North"="European hake, NE Atlantic N",                                  
                                         "Hake Northeast Atlantic South"="European hake, NE Atlantic S",                                   
                                         "Horse mackerel IIa-IVa-Vb-VIa-VII-VIII"="Horse mackerel, NE Atlantic",                          
                                         "Mackerel ICES Northeast Atlantic"="Mackerel, NE Atlantic",                          
                                         "Monkfish Southern Georges Bank / Mid-Atlantic"="Monkfish, GB/Mid-Atlantic",               
                                         "Northern fur seal San Miguel Islands [pups]"="Northern fur seal, San Miguel Isl",                      
                                         "Northern fur seal St. George"="Northern fur seal St. George Isl",                                     
                                         "Northern fur seal St. Paul"="Northern nur seal, St. Paul Isl",                                      
                                         "Pacific bluefin tuna Pacific Ocean"="Pacific bluefin tuna, Pacific",                            
                                         "Peruvian booby Peru (number of adults)"="Peruvian booby, Peru",                        
                                         "Peruvian pelican Peru (number of adults)"="Peruvian pelican, Peru",                        
                                         "Pacific harbor seal Juan de la Fuca [abundance (total est.)]"="Pacific harbor seal, Juan de Fuca",  
                                         "Pacific harbor seal Oregon [abundance (total est.)]"="Pacific harbor seal, Oregon",         
                                         "Pacific harbor seal San Juan Islands [abundance (total est.)]"="Pacific harbor seal, San Juan Isl",  
                                         "Pacific harbor seal Coastal Estuaries [abundance (total est.)]"="Pacific harbor seal, WA estuaries",  
                                         "Pollock NAFO-5YZ"="Pollock NAFO 5YZ",                                               
                                         "Pollock ICES IIIa, VI and North Sea"="Pollock ICES NS-IIa-VI",                          
                                         "Silver hake Scotian Shelf and Bay of Fundy"="Silver hake, Scotian Shelf",                     
                                         "Shortspine thornyhead Pacific Coast"="SS thornyhead, US West Coast",                         
                                         "Striped marlin Northeast Pacific"="Striped marlin, NE Pacific",                             
                                         "White hake Southern Gulf of St. Lawrence"="White hake, G of St Lawrence",                         
                                         "White hake Georges Bank / Gulf of Maine"="White hake, GB/GOM",                         
                                         "Whiting NS-VIId"="Whiting ICES NS-VIId",                                            
                                         "White marlin Atlantic"="White marlin, Atlantic",                                        
                                         "Yellowtail rockfish Northern Pacific Coast"="Yellowtail rockfish, US West Coast")))

# Plot data
################################################################################

# Plotting params
models <- c("re", "fe")
titles <- c("Random effects model", "Fixed effects model")
xlim <- c(1, 4)

# Setup figure
figname <- "Fig1_prey1_influence.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=4, units="in", res=600)
layout(matrix(c(1,2,3,4,
                1,2,3,5,
                1,2,3,6), ncol=4, byrow=T), widths=c(0.70/4, 0.70/3, 0.70/3, 0.3))
par(mar=c(1,0.5,0.9,0.5), mgp=c(2.8,0.8,0), oma=c(2.5,0,0,0))

# Emptyplot
freeR::emptyplot()

# Loop through models
for(i in 1:length(models)){
  
  # Prep data for plotting
  if(i==1){
    data <- select(combo, stocklong1, type, tcolor, betaT_re, betaT_lo_re, betaT_hi_re, pcolor_re, lcolor_re) %>% 
      rename(betaT=betaT_re, betaT_lo=betaT_lo_re, betaT_hi=betaT_hi_re, pcolor=pcolor_re, lcolor=lcolor_re) %>% 
      mutate(pcolor=as.character(pcolor),
             lcolor=as.character(lcolor))
  }else{
    data <- select(combo, stocklong1, type, tcolor, betaT_fe, betaT_lo_fe, betaT_hi_fe, pcolor_fe, lcolor_fe) %>% 
      rename(betaT=betaT_fe, betaT_lo=betaT_lo_fe, betaT_hi=betaT_hi_fe, pcolor=pcolor_fe, lcolor=lcolor_fe) %>% 
      mutate(pcolor=as.character(pcolor),
             lcolor=as.character(lcolor))
  }
  
  # Setup empty plot
  plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=0.9,
       xlim=c(-1*xlim[i], xlim[i]), ylim=c(1,nrow(data)+2),
       xlab="", ylab="", main=titles[i])
  
  # Calculate plotting positions
  # (breaks between groups)
  gap <- 1
  mpos <- 1:sum(data$type=="mammals")
  bpos <- max(mpos) + gap + 1:sum(data$type=="birds")
  fpos <- max(bpos) + gap + 1:sum(data$type=="fish")
  ypos <- c(mpos, bpos, fpos)
  
  # Add theta estimates (points) and errors (lines)
  sapply(1:nrow(data), function(x) lines(x=c(data$betaT_lo[x], data$betaT_hi[x]), y=c(ypos[x],ypos[x]), col=data$lcolor[x]))
  points(data$betaT, ypos, pch=16, cex=0.8, col=data$pcolor)
  
  # Add theta=0 line
  lines(x=c(0,0), y=c(1, max(ypos)), lty=3, col="black", lwd=1.5)
  
  # Add predator labels
  if(i==1){
    text(x=xlim[i]*-1, y=ypos, labels=data$stocklong1, cex=0.7, xpd=NA, pos=2, col=as.character(data$tcolor), xpd=NA)
  }

  # # Add prey labels
  # text(x=xlim[i], y=ypos, offset=-1, labels=data$prey1, cex=0.65, xpd=NA, pos=2, col="grey70")

  
}

# Add x-axis label
# xlabel <- expression("SST influence (θ"["i"]*")")
xlabel <- "Influence of primary prey"
mtext(xlabel, outer=T, side=1, adj=0.40, line=1.2, cex=0.8)


# Example populations
#######################################

# Load data
load(file.path(datadir, "pella_best_fixed_prey1.Rdata"))

# Example stocks
ex_stocks <- c("SHAKE4VWX", "CGANNETWCAPE", "PHSEALJFUCA")
ex_text <- c("Silver hake, Scotian Shelf", "Cape gannet, South Africa", "Harbor seal, Juan de Fuca")

# Parameters
p <- 0.55
xmin <- 0
xmax <- 1
ymin <- c(-0.1, -0.8, -0.4)
ymax <- c(0.4, 0.6, 0.4)
ybin <- c(0.1, 0.2, 0.2)

# Plotting par
par(mar=c(1,3.5,0.5,0.5), mgp=c(2,0.7,0))

# Loop through example stocks
for(i in 1:length(ex_stocks)){
  
  # Stock info
  stock <- filter(fe, stockid==ex_stocks[i])
  r <- stock$r
  k <- stock$k
  # k <- k_orig * stock$tb_max / 1000
  betaT <- stock$betaT
  
  # Subset data
  sdata <- data %>%
    filter(stockid==ex_stocks[i]) %>%
    mutate(sp=sp/1000,
           tb=tb/1000)
  
  # Add color bins to data
  range(sdata$prey1_b_sd)
  sdata$sst_bin <- cut(sdata$prey1_b_sd, breaks=seq(-2.7,2.7,0.1))
  pcolors <- tcolor(rev(colorpal(brewer.pal(11,"PiYG"),nlevels(sdata$sst_bin))), 0.8)
  sdata$sst_col <- pcolors[sdata$sst_bin]
  
  # Plot data
  # ymin <- floor(min(sdata$sp_sd) / 0.1) * 0.1
  # ymax <- ceiling(max(sdata$sp_sd) / 0.1) * 0.1
  plot(sp_sd ~ tb_sd, sdata, bty="n", las=1, cex=1.5, pch=21, bg=sdata$sst_col, #bg="grey80",
       xaxt="n", yaxt="n",
       xlim=c(0,1), 
       ylim=c(ymin[i], ymax[i]), 
       cex.lab=1.2, cex.axis=0.9,
       xlab="", ylab="", xpd=NA)
  if(i==3){
    axis(1, at=seq(0,1,0.2), labels=T, las=1, cex.axis=0.9)
  }else{
    axis(1, at=seq(0,1,0.2), labels=F, las=1, cex.axis=0.9)
  }
  axis(2, at=seq(ymin[i], ymax[i], ybin[i]), las=1, cex.axis=0.9)
  
  # Add SP curve
  curve(r/p*x*(1-(x/k)^p),
        from=0, to=1, n=101, add=T, lty=1, lwd=1.8, col="black")

  # Add SP-SST curves
  ssts <- c(-1, -0.5, 0.5, 1)
  colors <- rev(brewer.pal(10, "PiYG"))[c(1,3,8,10)]
  for(j in 1:length(ssts)){
    curve(r/p*x*(1-(x/k)^p)*exp(betaT*ssts[j]),
          from=0, to=1, n=101, add=T, lty=1, lwd=1.3, col=colors[j])
  }

  # # Add degree labels
  # if(i==1){
  #   text(x=4.5, y=4, pos=2, labels="+1°C", cex=0.9, col=colors[4])
  #   text(x=4, y=1.3, pos=4, labels="-1°C", cex=0.9, col=colors[1])
  # }
  # if(i==2){
  #   text(x=10, y=11.5, pos=2, labels="-1°C", cex=0.9, col=colors[1])
  #   text(x=20, y=1, pos=4, labels="+1°C", cex=0.9, col=colors[4])
  # }

  # Add warming effect label
  x <- 0-xmax*0.05
  y <- ymax[i]+(ymax[i]-ymin[i])*0.05
  text(x=x, y=y, labels=ex_text[i], pos=4,  cex=0.9, col="black", font=2, xpd=NA)

  # # Add letter
  # text(x=xmax, y=ymax, labels=LETTERS[i+2], pos=1, font=2, cex=1.1, offset=0.2)
  
  
}

# Axis labels
mtext("Biomass (scaled)", outer=T, side=1, adj=0.94, line=1.2, cex=0.8)
mtext("Production (scaled)", outer=T, side=2, adj=0.52, line=-34, cex=0.8)

#######################################
dev.off()