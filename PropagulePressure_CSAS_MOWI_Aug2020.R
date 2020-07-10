#libraries -------------used in Freyer's function
library(gdistance)
library(maps)
library(mapdata)
library(maptools)
library(marmap)
library(PBSmapping) #
library(ggplot2)
library(dplyr)
library(data.table)
library(gtable)
library(vegan)
library(cluster)
library(geosphere)
library(plyr)
library(lubridate)
library(sp)
library(ggmap)
library(reshape2)
library(rgdal)
library(raster)
library(rgeos)
library(grid)
library(gridExtra)
library(ggrepel)
library(scales)
library(ggspatial)
library(AQpress)
library(car)
library(effects)
library(ggsn)
library(raster)

#R script used to calculate propagule pressure for CSAS Marine Harvest Atlantic Canada (Mowi) - Aquaculture Baseline Site Assessments Review - August 5-7, 2020


#This R Script is modified from AQPress function  on Github - primarily editted to make calculations of least cost distances faster by using NOAA bathy data from marmap
setwd("~/Desktop/Sarah/Salmon/CSAS_PropPressure/")

#River coordinates
river_coords=read.csv("AdjustedCoords_WildSites.csv", header=T)

#Keep just Lat/Long and site info
river_coords2=river_coords[,c(1,5,6)]
colnames(river_coords2)=c("River", "Lat", "Long")
river_coords2$prov=c("NL")
colnames(river_coords2)=c("River","Lat", "Long", "prov")


#Use this info for aqua site coordinates (different from aquasites because points moved  off land)
#Aquaculture sites and inventory
#Note - these same sites were used for calculation propagule pressure under proposed scenario with the addition of the three new aquaculture sites (with biomass of 1 million fish)
#This file contains all current aquaculture sites (a total of 50 sites)
aqua=read.csv("All_Sites_invetory_adjLatLong.csv", header=T)
aqua$Year=c(2019)
aqua$Province=c("NL")
aqua$totalfish=aqua$Max.Licensed.Stocking

#Inventory info (number of fish)
inventor_aqua=aqua[,c(1,6,7,8, 9, 10)] #Use this for location and inventory information
colnames(inventor_aqua)=c("Site.ID", "Lat", "Long", "Year", "prov", "totalfish")


#Analyze all sites?
AQsites=inventor_aqua[,c(1:5)]
nrow(AQsites)

rivercoords = river_coords2
inventory=inventor_aqua[,c(1:6)]
nrow(inventory)


#Most of this next part is same as AQPress function from Freya Keyser but modified the map used to get distances (https://github.com/freyakeyser/AQpress)
#dir.create("ResultsCSAS_2020/")
dir="ResultsCSAS_2020/"
distType="weightedDist"
stocking=T

options(scipen = 999)

#Edit for this one site distance matrix - make sure it is in water (not on land)
AQsites[12,]
AQsites$Lat[which(AQsites$Site.ID=="Lou Cove")]<- 47.42343
AQsites$Long[which(AQsites$Site.ID=="Lou Cove")]<- -56.07977

#start of Freyers script to prepare data objects with site info
AQsites <- subset(AQsites, select=c(Site.ID, Lat, Long, prov))
rivercoords <- subset(rivercoords, select=c(River, Lat, Long))
inventory <- subset(inventory, select=c(Site.ID, Lat, Long, Year, prov, totalfish))

AQsites$Site.ID <- as.character(AQsites$Site.ID)
AQsites$Lat <- as.numeric(as.character(AQsites$Lat))
AQsites$Long <- as.numeric(as.character(AQsites$Long))

rivercoords$River <- as.character(rivercoords$River)
rivercoords$Lat <- as.numeric(as.character(rivercoords$Lat))
rivercoords$Long <- as.numeric(as.character(rivercoords$Long))

inventory$Site.ID <- as.character(inventory$Site.ID)
inventory$Lat <- as.numeric(as.character(inventory$Lat))
inventory$Long <- as.numeric(as.character(inventory$Long))
inventory$prov <- as.character(inventory$prov)
inventory$totalfish <- as.numeric(as.character(inventory$totalfish))

alllong <- c(AQsites$Long, rivercoords$Long)
alllat <- c(AQsites$Lat, rivercoords$Lat)
extentAQ <- c(min(alllong)-1, max(alllong)+1, min(alllat)-1, max(alllat)+1)

#Use NOAA bathy instead
#Use extentAQ to determine max/min Lat Longs for bathy map
bathy<-getNOAA.bathy(-61, -50, 44, 49, resolution = 2, keep = T)
#Note got different distances after updating marmap recently - so results may vary from previous estimates

#least cost distance between sites - start with transition matrix
trans1 <- trans.mat(bathy)

#Can use this to get least cost distance with bathy transition matrix - same function as Freya's
dAQ <- costDistance(trans1, fromCoords = cbind(as.numeric(AQsites$Long), as.numeric(AQsites$Lat)),
                    toCoords = cbind(as.numeric(rivercoords$Long), as.numeric(rivercoords$Lat)))


#Check to see if any sites on land (high value indicates on land)
dAQ <- as.data.frame(dAQ)

#View(dAQ)
#head(dAQ)
test1 <- colwise(mean)
vals1 <- which(test1(dAQ) >3000000)
vals2 <- apply(dAQ[,1:length(rivercoords$Long)], 2, function(x) mean(x))
vals2 <- which(vals2 > 3000000)

#Checking to see if there are values > 3000000 which may indicate that the aquaculture site or the river site are on land (so estimates are incorrect)
if (length(vals1) > 0) {
  print(rivercoords[vals1,])
}
if (length(vals2) > 0) {
  print(AQsites[vals2,])
}
if (length(vals1) | length(vals2) > 0)
  stop("Some points are over 3000 km apart, which suggests that points may be on land. Use plot(rasterAQ) and click() to adjust placement of points in AQsites and rivercoords data.frames manually prior to running calcAQpress(). Check printed dataframe above for potential failed coordinates.")

#Max distance is ~4000 KM (which makes sense for Spain to Top of Norway)
#If all points are in water (least cost distances are reasonable...) then continue with script
max(dAQ)
#View(dAQ)

#Create dataframe and add River and Aquaculture site IDs
dAQ <- as.data.frame(dAQ)
colnames(dAQ) <- rivercoords$River
dAQ <- cbind(dAQ, AQsites$Site.ID, as.character(AQsites$prov))
colnames(dAQ)[dim(dAQ)[2] - 1] <- "Site.ID"
colnames(dAQ)[dim(dAQ)[2]] <- "prov"


#Save distance matrix if needed later
write.table(dAQ, "Distance_Matrix_Aqua-Rivers_CSAS2020_CurrentData.txt", quote = F,  col.names = T, row.names = F, sep="\t")


#Calculate weighted distances
dist2AQ <- cbind(dAQ, inventory)
dist2AQ <- subset(dist2AQ, is.na(totalfish)==FALSE)
#Joined inventory data"

dist2AQ[,1:length(rivercoords$Long)] <- apply(dist2AQ[,1:length(rivercoords$Long)], 2, function(x) as.character(x))
dist2AQ[,1:length(rivercoords$Long)] <- apply(dist2AQ[,1:length(rivercoords$Long)], 2, function(x) as.numeric(x))

#Set values with 0 to 1000 (which is 1 km) - this is because distance is in the denominator
dist2AQ[dist2AQ == 0] <- 1000


# change totalfish to 1 (1 year) - not using actual biomass data (just using presence/absence of aquaculture)
#Not used in these calculations
#If presence/absence is used (i.e. 1 fish) then propagule pressure will be much smaller - 
#     --- for example, if biomass is 1 Million -- calculation iwth biomass is 1 million fish/distance, whereas presence/absence will be 1 fish/distance
if(stocking=="FALSE"){
  dist2AQ$totalfish <- 1}


# For each site, caclulate total fish/distance from river
fishdist <- c(dist2AQ$totalfish/dist2AQ[,1:length(rivercoords$Long)])
dist2AQtest <- cbind(Year=dist2AQ$Year, data.frame(fishdist))

dist2AQmelt <- melt(data=dist2AQtest, id.vars = "Year", variable.name = "River")

# For each year and river, calculate total propagule pressure
prop.press <- ddply(.data=dist2AQmelt, .(Year, River),
                    summarize,
                    prop.press=sum(value, na.rm=T))
#Saved results by year - note that we only used one year of data here

prop.press_final <- join(prop.press, rivercoords, type="left")

save1 <- paste0(dir, "/prop.press_final_", Sys.Date(), ".csv")
write.csv(x = prop.press_final, file = save1)

#Calculate and save average propagule pressure for each river
#This will not be different if only one year is used
prop.press_avg <- ddply(.data=prop.press_final, .(River, Lat, Long),
                        summarize,
                        averagepress = mean(prop.press),
                        nyears = length(unique(Year)),
                        stdpress = sd(prop.press))

save2 <- paste0(dir, "/prop.press_avg_", Sys.Date(), ".csv")

write.csv(x = prop.press_avg, file = save2)

save.image("aqua_dat_CSAS_2020_CurrentData.RData")

