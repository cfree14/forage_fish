

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/trade_and_collapse/data/fao_landings/data"
plotdir <- "figures"

# Read data
load(file.path(datadir, "1950_2017_FAO_landings_data_all.Rdata"))


# Build
################################################################################

# FAMILIES
# Herrings/shads/sardines/sardinella/menhadens/sprat (Clupeidae), 
# anchovies (Engraulidae), sandeels (Ammodytidae), sauries (Scomberesocidae), 
# capelin (Osmeridae), illisha (Pristigasteridae)
# GENERA
# True mackerels (Scomber spp. and Rastrelliger spp.), Jack mackerels (Trachurus spp.)
# SPECIES
# Blue whiting (Micromesistius poutassou), Norway pout (Trisopterus esmarkii)

# Subset forage fish species
ff_families <- c("Clupeidae", "Engraulidae", "Ammodytidae", "Scomberesocidae", "Osmeridae", "Pristigasteridae")
ff_genera <- c("Scomber", "Rastrelliger", "Trachurus")
ff_species <- c("Micromesistius poutassou", "Trisopterus esmarkii")
ff_key <- taxa_key %>% 
  filter(family %in% ff_families | genus %in% ff_genera | sciname %in% ff_species) %>% 
  left_join(unique(select(landings, sci_name, comm_name)), by=c("sciname"="sci_name"))
ff_spp <- ff_key$sciname

# Landings
data <- landings %>% 
  filter(prod_short=="WC" & area_type=="marine") %>%
  left_join(select(taxa_key, sciname, family), by=c("sci_name"="sciname")) %>% 
  mutate(ff=sci_name %in% ff_spp) %>% 
  group_by(year) %>% 
  summarize(tl=sum(tl_mt)/1E6,
            tl_ff=sum(tl_mt[ff==T])/1E6,
            ff_perc=tl_ff/tl)



# Plot data
plot(tl ~ year, data, bty="n", las=1, type="l", xlim=c(1950, 2020),
     ylim=c(0,100), xlab="", ylab="Landings (millions mt)")
lines(data$year, data$tl_ff, col="red")


# Plot data
################################################################################

# Setup figure
figname <- "Fig0_ff_landings.png"
png(file.path(plotdir, figname), width=5, height=3, units="in", res=600)
par(mar=c(3,4,0.5,2.5), mgp=c(2.5,0.8,0))

# Plot data
plot(tl ~ year, data, bty="n", las=2, type="n", xlim=c(1950, 2020),
     ylim=c(0,100), xlab="", ylab="Landings (millions mt)")

# Add total shading
polygon(x=c(data$year, rev(data$year)), y=c(rep(0, nrow(data)), rev(data$tl)), col="grey80", border=NA)
polygon(x=c(data$year, rev(data$year)), y=c(rep(0, nrow(data)), rev(data$tl_ff)), col="grey30", border=NA)

# Label years
tl_last <- data$tl[data$year==2015]
tl_ff_last <- data$tl_ff[data$year==2015]
tl_last_text <- paste0(format(round(tl_last, 1), nsmall=1), " million mt\nall fish")
tl_ff_last_text <- paste0(format(round(tl_ff_last, 1), nsmall=1), " million mt\nforage fish")
text(x=2015, y=tl_last, pos=4, label=tl_last_text, cex=0.75, offset=0.2, col="grey60", xpd=NA)
text(x=2015, y=tl_ff_last, pos=4, label=tl_ff_last_text, cex=0.75, offset=0.2, col="grey10", xpd=NA)

# Off
dev.off()
graphics.off()

# Print 2015 percentage
print(data$ff_perc[data$year==2015])

