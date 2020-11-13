# connecting Walz et al. 2015 through ArcGIS and R for the Senegal River mouth

## Habitat variable 2: water temperature

### Water temperature function for a) S. mansoni

SmWaterTemp <- function(Temp){ifelse(Temp<16, 0,ifelse(Temp<=35, -0.003 * (268/(Temp - 14.2) - 335), 0))}
Temp <- seq(10,40, by=1)
plot(Temp, SmWaterTemp(Temp), type = 'l', lwd = 2,ylim=c(0,1),main='a) Water temperature function for S. mansoni',xlab='Water surface temp C',ylab='Relative suitability')

### Water temperature function for b) S. haematobium

ShWaterTemp <- function(Temp){ifelse(Temp<17, 0,ifelse(Temp<=33, -0.006 * (295/(Temp - 15.3) - 174), 0))}
Temp <- seq(10,40, by=1)
plot(Temp, ShWaterTemp(Temp), type = 'l', lwd = 2,ylim=c(0,1),main='b) Water temperature function for S. haematobium',xlab='Water surface temp C',ylab='Relative suitability')

### Water temperature function for c) Biomphalaria spp.

BglabrataWaterTemp <- function(Temp){ifelse(Temp<16, 0,ifelse(Temp<=35, -4.095 + 0.368 * Temp - 0.007 * Temp^2, 0))}
Temp <- seq(10,40, by=1)
plot(Temp, BglabrataWaterTemp(Temp), type = 'l', lwd = 2,ylim=c(0,1),main='c) Water temperature function for Biomphalaria spp.',xlab='Water surface temp C',ylab='Relative suitability')

### Water temperature function for d) Bulinus truncatus

BtruncatusWaterTemp <- function(Temp){ifelse(Temp<17, 0,ifelse(Temp<=33, -2.350 + 0.208 * Temp - 0.004 * Temp^2, 0))}
Temp <- seq(10,40, by=1)
plot(Temp, BtruncatusWaterTemp(Temp), type = 'l', lwd = 2,ylim=c(0,1),main='d) Water temperature function for Bulinus spp.',xlab='Water surface temp C',ylab='Relative suitability')

### Read in .dbf table of October water surface temp

library(foreign)
octWST_table<-read.dbf(file="C:/NINA_R/octWST/OctWSTpos.dbf",as.is=FALSE)

### Check the data with these commands

ls(octWST_table)
head(octWST_table)
str(octWST_table)

### Visualize the data with a histogram

oct_temp<-octWST_table$grid_code
hist(oct_temp)

### calculate Relative Suitability Water Temperature in October for S. mansoni

REL_SUIT_OCT_WSTSm<-SmWaterTemp(octWST_table$grid_code)
head(REL_SUIT_OCT_WSTSm)
str(REL_SUIT_OCT_WSTSm)

### Add a calculated column for Relative Suitability Water Temperature in October for S. mansoni to the dataframe

octWST_table$REL_SUIT_OCT_WSTSm<-SmWaterTemp(octWST_table$grid_code)

### Visualize the Relative Suitability Water Temperature in October for S. mansoni as a histogram

hist(REL_SUIT_OCT_WSTSm)

### calculate Relative Suitability Water Temperature in October for S. haematobium

REL_SUIT_OCT_WSTSh<-ShWaterTemp(octWST_table$grid_code)
head(REL_SUIT_OCT_WSTSh)
str(REL_SUIT_OCT_WSTSh)

### Add a calculated column for Relative Suitability Water Temperature in October for S. haematobium to the dataframe

octWST_table$REL_SUIT_OCT_WSTSh<-ShWaterTemp(octWST_table$grid_code)

### calculate Relative Suitability Water Temperature in October for Biomphalaria glabrata

REL_SUIT_OCT_WSTBtruncatus<-BtruncatusWaterTemp(octWST_table$grid_code)
head(REL_SUIT_OCT_WSTBtruncatus)
str(REL_SUIT_OCT_WSTBtruncatus)

### Add a calculated column for Relative Suitability Water Temperature in October for Biomphalaria glabrata to the dataframe

octWST_table$REL_SUIT_OCT_WSTBtruncatus<-BtruncatusWaterTemp(octWST_table$grid_code)

### calculate Relative Suitability Water Temperature in October for Bulinus truncatus

REL_SUIT_OCT_WSTBglabrata<-BglabrataWaterTemp(octWST_table$grid_code)
head(REL_SUIT_OCT_WSTBglabrata)
str(REL_SUIT_OCT_WSTBglabrata)

### Add a calculated column for Relative Suitability Water Temperature in October for Bulinus truncatus to the dataframe

octWST_table$REL_SUIT_OCT_WSTBglabrata<-BglabrataWaterTemp(octWST_table$grid_code)

### Check the data with these commands

ls(octWST_table)
head(octWST_table)
str(octWST_table)

### Write .dbf file including the relative suitability

write.dbf(octWST_table,file="C:/NINA_R/octWST/OctWSTSuit.dbf")

### Read in .dbf table of June water surface temp

library(foreign)
junWST_table<-read.dbf(file="C:/NINA_R/junWST/JunB10B11WST_watermask.dbf",as.is=FALSE)

### Check the data with these commands

ls(junWST_table)
head(junWST_table)
str(junWST_table)

### Visualize the data with a histogram

jun_temp<-junWST_table$grid_code
hist(jun_temp)

### calculate Relative Suitability Water Temperature in June for S. mansoni

REL_SUIT_JUN_WSTSm<-SmWaterTemp(junWST_table$grid_code)
head(REL_SUIT_JUN_WSTSm)
str(REL_SUIT_JUN_WSTSm)

### Add a calculated column for Relative Suitability Water Temperature in June for S. mansoni to the dataframe

junWST_table$REL_SUIT_JUN_WSTSm<-SmWaterTemp(junWST_table$grid_code)

### Visualize the Relative Suitability Water Temperature in June for S. mansoni as a histogram

hist(REL_SUIT_JUN_WSTSm)

### calculate Relative Suitability Water Temperature in June for S. haematobium

REL_SUIT_JUN_WSTSh<-ShWaterTemp(junWST_table$grid_code)
head(REL_SUIT_JUN_WSTSh)
str(REL_SUIT_JUN_WSTSh)

### Add a calculated column for Relative Suitability Water Temperature in June for S. haematobium to the dataframe

junWST_table$REL_SUIT_JUN_WSTSh<-ShWaterTemp(junWST_table$grid_code)

### calculate Relative Suitability Water Temperature in June for Biomphalaria glabrata

REL_SUIT_JUN_WSTBtruncatus<-BtruncatusWaterTemp(junWST_table$grid_code)
head(REL_SUIT_JUN_WSTBtruncatus)
str(REL_SUIT_JUN_WSTBtruncatus)

### Add a calculated column for Relative Suitability Water Temperature in June for Biomphalaria glabrata to the dataframe

junWST_table$REL_SUIT_JUN_WSTBtruncatus<-BtruncatusWaterTemp(junWST_table$grid_code)

### calculate Relative Suitability Water Temperature in June for Bulinus truncatus

REL_SUIT_JUN_WSTBglabrata<-BglabrataWaterTemp(junWST_table$grid_code)
head(REL_SUIT_JUN_WSTBglabrata)
str(REL_SUIT_JUN_WSTBglabrata)

### Add a calculated column for Relative Suitability Water Temperature in June for Bulinus truncatus to the dataframe

junWST_table$REL_SUIT_JUN_WSTBglabrata<-BglabrataWaterTemp(junWST_table$grid_code)

### Check the data with these commands

ls(junWST_table)
head(junWST_table)
str(junWST_table)

### Write .dbf file including the relative suitability

write.dbf(junWST_table,file="C:/NINA_R/junWST/JunWSTSuit.dbf")

## Habitat variable 4: water depth

### Water depth function

WaterDepth <- function(shoredist){ifelse(shoredist<=210,-0.0043*shoredist+1,ifelse(shoredist<=2000,-0.000056*shoredist + 0.088,0))}
shoredist <- seq(0,3000, by=1)
plot(shoredist, WaterDepth(shoredist), type = 'l', lwd = 2,ylim=c(0,1),main='e) Water depth function',xlab='Horizontal distance from shore (m)',ylab='Relative suitability')

### Read in .dbf table of October water depth

library(foreign)
octDepth_table<-read.dbf(file="C:/NINA_R/WaterDepthOct/WaterDepthOctMasked.dbf",as.is=FALSE)

### Check the data with these commands

ls(octDepth_table)
head(octDepth_table)
str(octDepth_table)

### Visualize the data with a histogram

octshoredist<-octDepth_table$grid_code
hist(octshoredist)

### calculate Relative Suitability Water Depth in October

REL_SUIT_OCT_Waterdepth<-WaterDepth(octDepth_table$grid_code)
head(REL_SUIT_OCT_Waterdepth)
str(REL_SUIT_OCT_Waterdepth)

### Add a calculated column for Relative Suitability Water Depth in October to the dataframe

octDepth_table$REL_SUIT_OCT_Waterdepth<-WaterDepth(octDepth_table$grid_code)

### Visualize the Relative Suitability Water Depth in October as a histogram

hist(REL_SUIT_OCT_Waterdepth)

### Check the data with these commands

ls(octDepth_table)
head(octDepth_table)
str(octDepth_table)

### Write .dbf file including the relative suitability

write.dbf(octDepth_table,file="C:/NINA_R/WaterDepthOct/OctWatDepSuit.dbf")

### Read in .dbf table of June water depth

library(foreign)
junDepth_table<-read.dbf(file="C:/NINA_R/WaterDepthJun/WaterDepthJuneMasked.dbf",as.is=FALSE)

### Check the data with these commands

ls(junDepth_table)
head(junDepth_table)
str(junDepth_table)

### Visualize the data with a histogram

junshoredist<-junDepth_table$grid_code
hist(junshoredist)

### calculate Relative Suitability Water Depth in June

REL_SUIT_JUN_Waterdepth<-WaterDepth(junDepth_table$grid_code)
head(REL_SUIT_JUN_Waterdepth)
str(REL_SUIT_JUN_Waterdepth)

### Add a calculated column for Relative Suitability Water Depth in June to the dataframe

junDepth_table$REL_SUIT_JUN_Waterdepth<-WaterDepth(junDepth_table$grid_code)

### Visualize the Relative Suitability Water Depth in June as a histogram

hist(REL_SUIT_JUN_Waterdepth)

### Check the data with these commands

ls(junDepth_table)
head(junDepth_table)
str(junDepth_table)

### Write .dbf file including the relative suitability

write.dbf(junDepth_table,file="C:/NINA_R/WaterDepthJun/JunWatDepSuit.dbf")

### check the new .dbf files

junDepthsuit_table<-read.dbf(file="C:/NINA_R/WaterDepthJun/JunWatDepSuit.dbf",as.is=FALSE)
summary(junDepthsuit_table)

## Habitat variable 5: vegetation coverage

### Vegetation coverage function

VegCoverage <- function(V){ifelse(V<0,0,ifelse(V<=0.3,3.33*V,1))}
V <- seq(-1,1, by=0.1)
plot(V, VegCoverage(V), type = 'l', lwd = 2,ylim=c(0,1),main='f) Vegetation coverage function',xlab='Normalized difference vegetation index (NDVI)',ylab='Relative suitability')

### Read in .dbf table of dry season NDVI

library(foreign)
dryvegsuit_table<-read.dbf(file="C:/NINA_R/dryseasonveg/Mask_dryNDVIwetM_xy.dbf",as.is=FALSE)

### Check the data with these commands

ls(dryvegsuit_table)
head(dryvegsuit_table)
str(dryvegsuit_table)

### Visualize the data with a histogram

dryNDVI<-dryvegsuit_table$grid_code
hist(dryNDVI)

### calculate Dry Season Vegetation Suitability

DRY_VEG_SUIT<-VegCoverage(dryvegsuit_table$grid_code)
head(DRY_VEG_SUIT)
str(DRY_VEG_SUIT)

### Add a calculated column for Dry Season Vegetation Suitability to the dataframe

dryvegsuit_table$DRY_VEG_SUIT<-VegCoverage(dryvegsuit_table$grid_code)

### Visualize the Dry Season Vegetation Suitability as a histogram

hist(DRY_VEG_SUIT)

### Check the data with these commands

ls(dryvegsuit_table)
head(dryvegsuit_table)
str(dryvegsuit_table)

### Write .dbf file including the relative suitability

write.dbf(dryvegsuit_table,file="C:/NINA_R/dryseasonveg/DryVegSuit.dbf")

### Read in .dbf table of wet season NDVI

library(foreign)
wetvegsuit_table<-read.dbf(file="C:/NINA_R/wetseasonveg/Mask_wetNDVIbuf.dbf",as.is=FALSE)

### Check the data with these commands

ls(wetvegsuit_table)
head(wetvegsuit_table)
str(wetvegsuit_table)

### Visualize the data with a histogram

wetNDVI<-wetvegsuit_table$grid_code
hist(wetNDVI)

### calculate Wet Season Vegetation Suitability

WET_VEG_SUIT<-VegCoverage(wetvegsuit_table$grid_code)
head(WET_VEG_SUIT)
str(WET_VEG_SUIT)

### Add a calculated column for Wet Season Vegetation Suitability to the dataframe

wetvegsuit_table$WET_VEG_SUIT<-VegCoverage(wetvegsuit_table$grid_code)

### Visualize the Wet Season Vegetation Suitability as a histogram

hist(WET_VEG_SUIT)

### Check the data with these commands

ls(wetvegsuit_table)
head(wetvegsuit_table)
str(wetvegsuit_table)

### Write .dbf file including the relative suitability

write.dbf(wetvegsuit_table,file="C:/NINA_R/wetseasonveg/WetVegSuit.dbf")

## Habitat variable 7: sink depth - not included

### Sink depth function

SinkDepth <- function(z){0.005*z+0.11}
z <- seq(1,222, by=1)
plot(z, SinkDepth(z), type = 'l', lwd = 2)

### Read in .dbf table of sink depth from ArcGIS



# connecting additional variables through ArcGIS and R for the Senegal River mouth

## Additional variable 1: depletion of native prawn predators

### Prawn depletion function

PrawnDep <- function(OceanDist){ifelse(OceanDist<400000, 0,ifelse(OceanDist<=1000000,0.000001667*OceanDist-0.667,1))}
OceanDist <- seq(0,1500000, by=1)
plot(OceanDist, PrawnDep(OceanDist), type = 'l', lwd = 2,ylim=c(0,1),main='g) Prawn depletion function',xlab='Distance from ocean (m)',ylab='Relative suitability')

### Prawn depletion function to plot (km instead of m)

PrawnDepkm <- function(OceanDist){ifelse(OceanDist<400, 0,ifelse(OceanDist<=1000,0.001667*OceanDist-0.667,1))}
OceanDist <- seq(0,1500, by=1)
plot(OceanDist, PrawnDepkm(OceanDist), type = 'l', lwd = 2,ylim=c(0,1),main='g) Prawn depletion function',xlab='Distance from estuary (km) in unobstructed river',ylab='Relative suitability')

#### with two caveats:
#### 1. applies only within the native range of the pawn species
#### 2. beyond a prawn-impenetrable dam, PrawnDep(OceanDist) steps immediately to 1

## Additional variable 2: social dimensions (proximity to water, rural population density, and urbanization)

### Proximity to water risk function

ProxRisk <- function(prox){ifelse(prox<1000, 1,ifelse(prox<=15000, -0.0000714 * prox + 1.0714,0))}
prox <- seq(0,20000, by=1)
plot(prox, ProxRisk(prox), type = 'l', lwd = 2,ylim=c(0,1),main='h) Proximity-to-water function',xlab='Distance from water body (m)',ylab='Relative proximity risk')

### Human population risk function - OLD COMBINED

#PopDenRisk <- function(h){ifelse(h<1,h,1/(1+exp((h-3)/0.4)))}
#h <- seq(0,10,0.1)
#plot(h, PopDenRisk(h), type = 'l', lwd = 2,ylim=c(0,1),main='i) Population density function',xlab='Human population (per hectare)',ylab='Relative population risk')

### Rural population density risk function

RuralDenRisk <- function(h){ifelse(h<1,h,1)}
h <- seq(0,10,0.1)
plot(h, RuralDenRisk(h), type = 'l', lwd = 2,ylim=c(0,1),main='i) Rural population density function',xlab='Human population (per hectare)',ylab='Relative density risk')

### Urbanization risk function

UrbanRisk <- function(h){ifelse(h<1,1,1/(1+exp((h-3)/0.4)))}
h <- seq(0,10,0.1)
plot(h, UrbanRisk(h), type = 'l', lwd = 2,ylim=c(0,1),main='j) Urbanization function',xlab='Human population (per hectare)',ylab='Relative urbanization risk')

### Human population risk function - OLD FROM GIULIO

#### Linear function of increasing # of humans at risk with increasing population density

#lines(h,h, type ='l', col='green')

#### Sigmoidal function of decreasing risk with increasing urbanization/urban water infrastructure

#PopDenRisk2 <- function(h){h/(1+exp((h-5)/1))}
#plot(h, PopDenRisk2(h), type = 'l', lwd = 2,ylab='Relative population risk', col='orange')

#RelRisk2<-PopDenRisk2(h)/max(PopDenRisk2(h))
#lines(h, RelRisk2, type ='l', col='black')

# PopatRisk <-PopDenRisk2(h)
# plot(h, PopatRisk, type = 'l', lwd = 2,ylab='Relative population risk', col='red')

#lines(h,1/(1+exp((h-5)/1)), type ='l', col='blue')

### Read in .dbf table of June human populations and proximity

library(foreign)
humanpopJun_table<-read.dbf(file="C:/NINA_R/pop_JuneWater/pop_JuneWater.dbf",as.is=FALSE)

### Check the data with these commands

ls(humanpopJun_table)
head(humanpopJun_table)
str(humanpopJun_table)

### Visualize the June proximity data with a histogram

junhumanprox<-humanpopJun_table$NEAR_DIST
hist(junhumanprox)

### calculate the June proximity risk

ProxRiskJun<-ProxRisk(humanpopJun_table$NEAR_DIST)
head(ProxRiskJun)
str(ProxRiskJun)

### Visualize the the June proximity risk as a histogram

hist(ProxRiskJun)

### Add a calculated column for June proximity risk to the dataframe

humanpopJun_table$ProxRiskJun<-ProxRisk(humanpopJun_table$NEAR_DIST)

### Check the data with these commands

ls(humanpopJun_table)
head(humanpopJun_table)
str(humanpopJun_table)

### Visualize the June population data with a histogram

junhumanpop<-humanpopJun_table$population
hist(junhumanpop,200)
min(junhumanpop)
max(junhumanpop)
mean(junhumanpop)

### calculate the June population risk - NEEDS TO BE UPDATED FOR RURAL POP DENS + URBANIZATION RISK

PopRiskJun<-PopDenRisk(humanpopJun_table$population)
head(PopRiskJun)
str(PopRiskJun)

### Visualize the the June population risk as a histogram

hist(PopRiskJun)
max(PopRiskJun)

### Add a calculated column for June population risk to the dataframe

humanpopJun_table$PopRiskJun<-PopDenRisk(humanpopJun_table$population)

### Check the data with these commands

ls(humanpopJun_table)
head(humanpopJun_table)
tail(humanpopJun_table)
str(humanpopJun_table)

### Calculate the June population-proximity risk as the geometric mean

PopProxRiskJun<-(ProxRiskJun*PopRiskJun)^0.5

### Visualize the the June population-proximity risk as a histogram

hist(PopProxRiskJun)
max(PopProxRiskJun)

### Add a calculated column for June population-proximity risk to the dataframe

humanpopJun_table$PopProxRiskJun<-(ProxRiskJun*PopRiskJun)^0.5

### Check the data with these commands

ls(humanpopJun_table)
head(humanpopJun_table)
tail(humanpopJun_table)
str(humanpopJun_table)
summary(humanpopJun_table)

### Write .dbf file including the June population-proximity risk

write.dbf(humanpopJun_table,file="C:/NINA_R/pop_JuneWater/PopProxRiskJun.dbf")

### Read in .dbf table of October human populations and proximity

library(foreign)
humanpopOct_table<-read.dbf(file="C:/NINA_R/pop_OctWater/pop_OctWater.dbf",as.is=FALSE)

### Check the data with these commands

ls(humanpopOct_table)
head(humanpopOct_table)
str(humanpopOct_table)

### Visualize the October proximity data with a histogram

octhumanprox<-humanpopOct_table$NEAR_DIST
hist(octhumanprox)

### calculate the October proximity risk

ProxRiskOct<-ProxRisk(humanpopOct_table$NEAR_DIST)
head(ProxRiskOct)
str(ProxRiskOct)

### Visualize the the October proximity risk as a histogram

hist(ProxRiskOct)

### Add a calculated column for October proximity risk to the dataframe

humanpopOct_table$ProxRiskOct<-ProxRisk(humanpopOct_table$NEAR_DIST)

### Check the data with these commands

ls(humanpopOct_table)
head(humanpopOct_table)
str(humanpopOct_table)

### Visualize the October population data with a histogram

octhumanpop<-humanpopOct_table$population
hist(octhumanpop,200)
min(octhumanpop)
max(octhumanpop)
mean(octhumanpop)

### calculate the October population risk

PopRiskOct<-PopDenRisk(humanpopOct_table$population)
head(PopRiskOct)
str(PopRiskOct)

### Visualize the the October population risk as a histogram

hist(PopRiskOct)
max(PopRiskOct)

### Add a calculated column for October population risk to the dataframe

humanpopOct_table$PopRiskOct<-PopDenRisk(humanpopOct_table$population)

### Check the data with these commands

ls(humanpopOct_table)
head(humanpopOct_table)
tail(humanpopOct_table)
str(humanpopOct_table)

### Calculate the October population-proximity risk as the geometric mean

PopProxRiskOct<-(ProxRiskOct*PopRiskOct)^0.5

### Visualize the the October population-proximity risk as a histogram

hist(PopProxRiskOct)
max(PopProxRiskOct)

### Add a calculated column for October population-proximity risk to the dataframe

humanpopOct_table$PopProxRiskOct<-(ProxRiskOct*PopRiskOct)^0.5

### Check the data with these commands

ls(humanpopOct_table)
head(humanpopOct_table)
tail(humanpopOct_table)
str(humanpopOct_table)
summary(humanpopOct_table)

### Write .dbf file including the June population-proximity risk

write.dbf(humanpopOct_table,file="C:/NINA_R/pop_OctWater/PopProxRiskOct.dbf")

## Additional variable 3: cattle density - not included

### Cattle density function

CattleRisk <- function(CattleDens){ifelse(CattleDens<100,0,1)}
CattleDens <- seq(0,200, by=1)
plot(CattleDens, CattleRisk(CattleDens), type = 'l', lwd = 2)

# Integrate the variables

# Tie the tables together into one dataframe based on the xy coordinates (one for wet and one for dry)
# make a unique ID for each row that is x and y concatenated
# 

#From Walz:
# m = count (the number of columns w/ relevant variables)
# fi = each relative suitability
# ai = 1/m
# HSI = Var1 Var2 Var3
# Overall Suitability(variables) = sum(variables * 1/m)
# Overall Suitability(variables) = sum(variable * 1/ [count the number of columns available for this pixel])
