

# Clear workspace
rm(list = ls())

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
sst <- read.csv(file.path(datadir, "pella_best_random_sst_results.csv"), as.is=T)
prey1 <- read.csv(file.path(datadir, "pella_best_random_prey1_results.csv"), as.is=T)

# Format data
################################################################################

# Format
combo <- sst %>% 
  # Reduce SST columns
  select(stockid, stocklong, type, betaT, betaT_lo, betaT_hi, betaT_inf) %>% 
  rename(betaT_sst=betaT, betaT_lo_sst=betaT_lo, betaT_hi_sst=betaT_hi, betaT_inf_sst=betaT_inf) %>% 
  mutate(pcolor_sst=as.character(revalue(betaT_inf_sst, c("negative"="red", "positive"="blue", "none"="black"))),
         lcolor_sst=as.character(revalue(betaT_inf_sst, c("negative"="red", "positive"="blue", "none"="grey60")))) %>% 
  # Add primary prey estimates
  left_join(select(prey1, stockid, betaT, betaT_lo, betaT_hi, betaT_inf), by="stockid") %>% 
  rename(betaT_prey1=betaT, betaT_lo_prey1=betaT_lo, betaT_hi_prey1=betaT_hi, betaT_inf_prey1=betaT_inf) %>% 
  mutate(pcolor_prey1=as.character(revalue(betaT_inf_prey1, c("negative"="red", "positive"="blue", "none"="black"))),
         lcolor_prey1=as.character(revalue(betaT_inf_prey1, c("negative"="red", "positive"="blue", "none"="grey60")))) %>% 
  # Arrange by type a FE estimate
  mutate(type=factor(type, levels=c("mammals", "birds", "fish"))) %>% 
  arrange(type, desc(betaT_sst)) %>% 
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
models <- c("sst", "prey1")
xlim <- c(2, 2)

# Setup figure
figname <- "FigX_sst_prey1_influence.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=4, units="in", res=600)
layout(matrix(c(1,2,3,
                1,2,3,
                1,2,3), ncol=3, byrow=T), widths=c(0.25, 0.75/2, 0.75/2))
par(mar=c(1,0.5,0.5,0.5), mgp=c(2,0.8,0), oma=c(2.5,0,0,0), xpd=NA)

# Emptyplot
freeR::emptyplot()

# Loop through models
for(i in 1:length(models)){
  
  # Prep data for plotting
  if(i==1){
    data <- select(combo, stocklong1, type, betaT_sst, betaT_lo_sst, betaT_hi_sst, pcolor_sst, lcolor_sst) %>% 
      rename(betaT=betaT_sst, betaT_lo=betaT_lo_sst, betaT_hi=betaT_hi_sst, pcolor=pcolor_sst, lcolor=lcolor_sst) %>% 
      mutate(pcolor=as.character(pcolor),
             lcolor=as.character(lcolor))
    xlab <- "Influence of warming"
  }else{
    data <- select(combo, stocklong1, type, betaT_prey1, betaT_lo_prey1, betaT_hi_prey1, pcolor_prey1, lcolor_prey1) %>% 
      rename(betaT=betaT_prey1, betaT_lo=betaT_lo_prey1, betaT_hi=betaT_hi_prey1, pcolor=pcolor_prey1, lcolor=lcolor_prey1) %>% 
      mutate(pcolor=as.character(pcolor),
             lcolor=as.character(lcolor))
    xlab <- "Influence of prey abundance"
  }
  
  # Setup empty plot
  plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=0.8,
       xlim=c(-1*xlim[i], xlim[i]), ylim=c(1,nrow(data)+2),
       xlab=xlab, ylab="")
  
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
    text(x=xlim[i]*-1-0.2, y=ypos, labels=data$stocklong1, cex=0.7, xpd=NA, pos=2, col="black", xpd=NA)
  }

  # # Add prey labels
  # text(x=xlim[i], y=ypos, offset=-1, labels=data$prey1, cex=0.65, xpd=NA, pos=2, col="grey70")

  
}

dev.off()