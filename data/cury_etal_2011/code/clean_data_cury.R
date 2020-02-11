
# Clean
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)

# Directories
outdir <- "data/cury_etal_2011"
datadir <- "data/cury_etal_2011/original"
plotdir <- "data/cury_etal_2011/figures"

# Read data
taxa <- read.csv(file.path(outdir, "cury_etal_2011_taxa.csv"), as.is=T)
diet_orig <- read.csv(file.path(datadir, "cury_etal_2011_diet_info.csv"), as.is=T)
pred_orig <- read.csv(file.path(datadir, "predator_abundance_data_template2 NB.csv"), as.is=T, na.strings=c("", " "))
prey_orig <- read.csv(file.path(datadir, "Biomass from Cury.csv"), as.is=T)


# Format diet data
################################################################################

# Format data
diet <- diet_orig %>% 
  rename(pred_comm=predator, prey_comm=prey, prop_occur=percent) %>% 
  mutate(source="Cury et al. (2011)", 
         dietid=paste(pred_comm, region),
         prop_occur=prop_occur/100,
         prop_use=prop_occur,
         prop_use_type=ifelse(!is.na(prop_occur), "occurence", "none")) %>% 
  select(source, dietid, everything())

# Check names
diet$pred_comm[!diet$pred_comm%in%taxa$comm_name]

# Format prey time series
################################################################################

# Format prey data
prey <- prey_orig %>% 
  # Rename columns
  rename(ref=Ref.Cury, year=Year, region=Main.LOC, location=Focused.LOC, 
         prey=Spp.Fish, n=Mean.Biomass, n_units=Unit, notes=Notes) %>% 
  # Format sci_name/comm_name
  mutate(comm_name=revalue(prey, c("anchovy"="European anchovy",
                                   "Euphausia superba"="Antarctic krill",
                                   "herring"="Atlantic herring",
                                   "Nyctiphanes australis"="Australian krill",
                                   "sand lance/capelin/pollock"="Sand lance/capelin/walleye pollock",
                                   "sardine"="Pacific sardine",
                                   "Sebastes spp "="Rockfish spp.")),
         sci_name=revalue(prey, c("anchovy"="Engraulis encrasicolus",
                                  "herring"="Clupea harengus",
                                  "sand lance/capelin/pollock"="Actinopterygii spp.",
                                  "sardine"="Sardinops sagax",
                                  "Sebastes spp "="Sebastes spp.")),
         region=revalue(region, c("CA"="California Current",
                                  "Benguela"="Benguela Current",
                                  "Cook Inlet"="Gulf of Alaska",
                                  "Scotia"="Scotia Sea",
                                  "Norway"="Norwegian Sea"))) %>% 
  # Format units and type
  mutate(type=notes,
         type=ifelse(grepl("MR", n_units), n_units, type),
         n_units=ifelse(grepl("MR", n_units), "index", n_units),
         n_units=revalue(n_units, c("individuals"="age 0 individuals",
                                    "Krill Index"="index"))) %>% 
  # Add stock name
  mutate(stocklong=trimws(paste(comm_name, location, type))) %>% 
  # Remove duplicated rows: Sand lance/capelin/walleye pollock Gull
  unique() %>% 
  # Arrange
  select(stocklong, region, location, comm_name, sci_name, year, n, n_units, type, ref) %>% 
  arrange(stocklong, year)

# Prey pops
prey_pops <- prey %>% 
  group_by(stocklong, region, location, comm_name, sci_name) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyrs=n(),
            dupyrs=anyDuplicated(year),
            n_units=paste(sort(unique(n_units)), collapse=", "),
            ref=paste(sort(unique(ref)), collapse=", ")) %>% 
  arrange(desc(nyrs))

# Check taxa
prey_pops$comm_name[!prey_pops$comm_name%in%taxa$comm_name]
prey_pops$sci_name[!prey_pops$sci_name%in%taxa$species]

# Check regions
table(prey_pops$region)


# Format Cury (non-JNCC) predator time series
################################################################################

# Format non-JNCC data
pred1 <- pred_orig %>% 
  # Subset to non-JNCC data
  filter(!grepl("JNCC", reference)) %>% 
  # Remove useless columns
  select(-c(Date, NB.Notes, catch, catch_units)) %>% 
  # Format year and species names
  mutate(year=as.numeric(year),
         comm_name=paste0(toupper(substr(comm_name, 1, 1)), substr(comm_name, 2, nchar(comm_name))),
         comm_name=revalue(comm_name, c("Rhino aucklet"="Rhinoceros auklet",
                                        "Red bill gull"="Red-billed gull",
                                        "Cape gannets"="Cape gannet",
                                        "Gentoo penguins"="Gentoo penguin",
                                        "Black leg kittiwake"="Black-legged kittiwake",
                                        "Common murre"="Common guillemot")),
         region=revalue(region, c("Benguela"="Benguela Current"))) %>% 
  # Format units
  mutate(n_units=tolower(n_units), 
         n_units=revalue(n_units, c("20-25000 range"="unknown", 
                                    "31688, roseneau et al 2000"="unknown",
                                    "breeding only; 6986 nishimoto and thomas 1991 in piatt 2002"="breeding only"))) %>% 
  # Add stock name
  mutate(stocklong=paste(comm_name, location, paste0("(", n_units, ")"))) %>% 
  # Arrange data and columns
  arrange(stocklong, year) %>% 
  select(stocklong, everything())

# Inspect
table(pred1$n_units)

# Inspect species
pred1_spp <- pred1 %>% 
  group_by(sci_name, comm_name) %>% 
  summarize(n=n())
pred1_spp$sci_name[!pred1_spp$sci_name%in%taxa$species_sub]
pred1_spp$comm_name[!pred1_spp$comm_name%in%taxa$comm_name]

# Inspect populations
pred1_pops <- pred1 %>%
  group_by(stocklong, region, location, comm_name) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            dupyrs=anyDuplicated(year),
            n_yr=n(),
            n_units=paste(sort(unique(n_units)), collapse=", "),
            reference=paste(sort(unique(reference)), collapse=", ")) %>% 
  left_join(taxa, by="comm_name") %>% 
  mutate(source="Cury et al. (2011)",
         dietid=paste(comm_name, region),
         area=region) %>% 
  select(source, stocklong, dietid, region, area, location, 
         type, class, order, family, species, species_sub, comm_name, 
         reference, n_units, n_yr) %>% 
  arrange(desc(n_yr))


# Format JNCC predator time series
################################################################################

# The JNCC data is super complicated and not usable without sophisticated analysis.

# Format JNCC data
pred2 <- pred_orig %>% 
  # Subset to non-JNCC data
  filter(grepl("JNCC", reference)) %>% 
  # Format year and species names
  mutate(comm_name=trimws(tolower(comm_name)),
         comm_name=paste0(toupper(substr(comm_name, 1, 1)), substr(comm_name, 2, nchar(comm_name))),
         comm_name=revalue(comm_name, c("Fulmar"="Northern fulmar",
                                        "Herring gull"="European herring gull",
                                        "Shag"="European shag")),
         sci_name=revalue(sci_name, c("Common Guillemot"="Uria aalge"))) %>% 
  # Add stock name
  mutate(stocklong=paste(comm_name, location)) %>% 
  # Arrange data and columns
  arrange(stocklong, year) %>% 
  select(stocklong, everything())

# Inspect species
pred2_spp <- pred2 %>% 
  group_by(sci_name, comm_name) %>% 
  summarize(n=n())

# Inspect populations
pred2_pops <- pred2 %>%
  group_by(stocklong) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyrs=n(),
            dupyrs=anyDuplicated(year),
            n_units=paste(sort(unique(n_units)), collapse=", "),
            ref=paste(sort(unique(reference)), collapse=", ")) %>% 
  arrange(desc(nyrs))


# Plot prey time series
################################################################################

# Setup figure
figname <- "Fig1_prey_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6,4), oma=c(2,4,2,2), mar=c(2,2,2,0.5))

# Plot data
for(i in 1:nrow(prey_pops)){
  
  # Subset data
  stock <- prey_pops$stocklong[i]
  sdata <- filter(prey, stocklong==stock)
  n_units <- prey_pops$n_units[i]
  
  # Plot data
  xmin <- floor1(min(sdata$year),10)
  xmax <- ceiling1(max(sdata$year),10)
  plot(n ~ year, type="b", sdata, bty="n", las=1, xlim=c(xmin, xmax), 
       xlab="", ylab=n_units, main=stock, cex.main=0.8, xpd=NA)
  
}

# Off
dev.off()
graphics.off()


# Plot predator time series
################################################################################

# Populations to use
pred1_pops_use <- filter(pred1_pops, n_yr>=15)

# Setup figure
figname <- "Fig2_pred_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6,4), oma=c(2,4,2,2), mar=c(2,2,2,0.5))

# Plot data
for(i in 1:nrow(pred1_pops_use)){
  
  # Subset data
  stock <- pred1_pops_use$stocklong[i]
  sdata <- filter(pred1, stocklong==stock)
  n_units <- pred1_pops_use$n_units[i]
  
  # Plot data
  xmin <- floor1(min(sdata$year),10)
  xmax <- ceiling1(max(sdata$year),10)
  plot(n ~ year, type="b", sdata, bty="n", las=1, xlim=c(xmin, xmax), 
       xlab="", ylab=n_units, main=stock)
  
}

# Off
dev.off()
graphics.off()


# Export data
################################################################################

# Export data
write.csv(diet, file=file.path(outdir, "cury_etal_2011_diet_props.csv"), row.names=F)
write.csv(prey, file=file.path(outdir, "cury_etal_2011_prey_time_series.csv"), row.names=F)
write.csv(prey_pops, file=file.path(outdir, "cury_etal_2011_prey_populations.csv"), row.names=F)
write.csv(pred1, file=file.path(outdir, "cury_etal_2011_predator_time_series.csv"), row.names=F)
write.csv(pred1_pops, file=file.path(outdir, "cury_etal_2011_predator_populations.csv"), row.names=F)

