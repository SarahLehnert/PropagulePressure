##Plotting map and barplots of results for propagule pressure
library(ggplot2)
library(maps) # tool for maps
library(mapdata) # all your basemaps are here
#library(ggrepel)
library(patchwork)

setwd("~/Desktop/Sarah/Salmon/CSAS_PropPressure/ResultsCSAS_2020/")

#Results of propagule pressure
#With current aquaculture sites
propPressure <-read.csv("prop.press_final_2020-07-10.csv", header=T)
#With current + proposed aquaculture sites
propPressure_withExpansion <-read.csv("prop.press_final_withProposed_2020-07-10.csv", header=T)

#Locations of rivers
locations <-read.csv("~/Desktop/Sarah/Salmon/CSAS_PropPressure/AdjustedCoords_WildSites.csv", header=T)
#Locations of all aquaculture sites (proposed and current)
aqua_sites<-read.csv("~/Desktop/Sarah/Salmon/CSAS_PropPressure/All_Sites_invetory_adjLatLong_CSAS2020_sitesAdded.csv")

#Add extra adjusted coordinates for plotting on map
locations$Longitude2 <- locations$Longitude
locations$Latitude2 <- locations$Latitude

#Adjust locations ofr these sites - so that all sites are more readily visble
locations$Longitude2[which(locations$River.1=="Taylor Bay Brook (Fortune Bay)")]=locations$Longitude[which(locations$River.1=="Taylor Bay Brook (Fortune Bay)")]-0.2 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Taylor Bay Brook (Fortune Bay)")]=locations$Latitude[which(locations$River.1=="Taylor Bay Brook (Fortune Bay)")]-0.2 #Change BSR pop by 0.5 deg longitude
locations$Longitude2[which(locations$River.1=="Tailrace Brook")]=locations$Longitude[which(locations$River.1=="Tailrace Brook")]-0.25 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Tailrace Brook")]=locations$Latitude[which(locations$River.1=="Tailrace Brook")]+0.05 #Change BSR pop by 0.5 deg longitude

locations$Longitude2[which(locations$River.1=="Southeast Brook")]=locations$Longitude[which(locations$River.1=="Southeast Brook")]+0.1 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Southeast Brook")]=locations$Latitude[which(locations$River.1=="Southeast Brook")]+0.2 #Change BSR pop by 0.5 deg longitude


locations$Longitude2[which(locations$River.1=="Conne River")]=locations$Longitude[which(locations$River.1=="Conne River")]+0.21#Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Conne River")]=locations$Latitude[which(locations$River.1=="Conne River")]+0.16 #Change BSR pop by 0.5 deg longitude


locations$Longitude2[which(locations$River.1=="Little River")]=locations$Longitude[which(locations$River.1=="Little River")]+0.05 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Little River")]=locations$Latitude[which(locations$River.1=="Little River")]-0.1 #Change BSR pop by 0.5 deg longitude


locations$Latitude2[which(locations$River.1=="Bay du Nord River")]=locations$Latitude[which(locations$River.1=="Bay du Nord River")]+0.2 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Northwest River")]=locations$Latitude[which(locations$River.1=="Northwest River")]+0.1 #Change BSR pop by 0.5 deg longitude


locations$Latitude2[which(locations$River.1=="Northeast River")]=locations$Latitude[which(locations$River.1=="Northeast River")]+0.12 #Change BSR pop by 0.5 deg longitude
locations$Longitude2[which(locations$River.1=="Northeast River")]=locations$Longitude[which(locations$River.1=="Northeast River")]+0.12 #Change BSR pop by 0.5 deg longitude

locations$Longitude2[which(locations$River.1=="Old Brook")]=locations$Longitude[which(locations$River.1=="Old Brook")]+0.1 #Change BSR pop by 0.5 deg longitude
locations$Latitude2[which(locations$River.1=="Old Brook")]=locations$Latitude[which(locations$River.1=="Old Brook")]-0.12 #Change BSR pop by 0.5 deg longitude


#Combine results with location information for both scenario (expansion and existing sites)
results <- cbind(propPressure,locations)
results_withExpansion <- cbind(propPressure_withExpansion,locations)

#Edit column names
colnames(results)<-c("x", "year","River1", "prop.press", "Lat", "Long", "River.2", "Latitude", "Longitude", "EstCount", "AdjLat", "AdjLong", "River", "Longitude2", "Latitude2")
colnames(results_withExpansion)<-c("x", "year","River1", "prop.press", "Lat", "Long", "River.2", "Latitude", "Longitude", "EstCount", "AdjLat", "AdjLong", "River","Longitude2", "Latitude2")

#Check min/max values of prop pressure for plotting
min(results$prop.press)
max(results$prop.press)
min(results_withExpansion$prop.press)
max(results_withExpansion$prop.press)

#Map for plotting results 
world <- map_data("world")
map_CurrentSites <- ggplot()+ 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", size=0.1, fill="gray50")+
  
    geom_point(data=aqua_sites[which(aqua_sites$Expansion.Site!="NEW"),],
             aes(x=Longitude, y=Latitude), fill="yellow",
             col="black", pch=24)+
  scale_x_continuous(limits = c(-60,-52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46,49), expand = c(0, 0)) +
  theme_bw()+theme(panel.grid = element_blank())+
  geom_segment(data= results, aes(x = Longitude, y = Latitude, xend = Longitude2, yend = Latitude2))+
  geom_point(data=results[order(results$prop.press, decreasing = T),],
             aes(x=Longitude2, y=Latitude2, fill=prop.press, size=prop.press),
             col="black", pch=21)+
  ylab("Latitude")+xlab("Longitude")+
  scale_size(limits = c(100, 3000), range=c(0.01,8))+
  scale_fill_gradientn(colours=c("blue", "red"), limits=c(100, 3000))+
  geom_point(aes(x=-59, y=46.5), fill="yellow",
             col="black", pch=24)+
  geom_text(aes(x=-58.85, y=46.5, hjust = 0, label="Existing sites"), )+
  #geom_text_repel(data=all_gen_offset, aes(x=Long.1, y=Lat, label=Rivername),
#        size=3)+
NULL  


map_with_Expansion <- ggplot()+ 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", size=0.1, fill="gray50")+
  geom_point(data=aqua_sites[which(aqua_sites$Expansion.Site!="NEW"),],
             aes(x=Longitude, y=Latitude), fill="yellow",
             col="black", pch=24)+
  geom_point(data=aqua_sites[which(aqua_sites$Expansion.Site=="NEW"),],
          aes(x=Longitude, y=Latitude), fill="skyblue",
        col="black", pch=24, size=5)+
  scale_x_continuous(limits = c(-60,-52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46,49), expand = c(0, 0)) +
  theme_bw()+theme(panel.grid = element_blank())+
  geom_segment(data= results_withExpansion, aes(x = Longitude, y = Latitude, xend = Longitude2, yend = Latitude2))+
  geom_point(data=results_withExpansion[order(results_withExpansion$prop.press, decreasing = T),],
             aes(x=Longitude2, y=Latitude2, fill=prop.press, size=prop.press),
             col="black", pch=21)+
  ylab("Latitude")+xlab("Longitude")+
  scale_size(limits = c(100, 3000), range=c(0.01,8))+
  scale_fill_gradientn(colours=c("blue", "red"), limits=c(100, 3000))+
  geom_point(aes(x=-59, y=46.5), fill="yellow",
             col="black", pch=24)+
  geom_text(aes(x=-58.85, y=46.5, hjust = 0, label="Existing sites"))+
  
  geom_point(aes(x=-59, y=46.35), fill="skyblue",
             col="black", pch=24, size=5)+
  geom_text(aes(x=-58.85, y=46.35, hjust = 0, label="Proposed sites"))+
  
  #geom_text_repel(data=all_gen_offset, aes(x=Long.1, y=Lat, label=Rivername),
  #        size=3)+
  NULL  

#Combined
map_CurrentSites /
map_with_Expansion


#Barplot showing propagule pressure values before and after expasion (current and proposed scenarios)
results_withExpansion$Scenario=c("Expansion")
results$Scenario=c("Current")

results_together<- rbind(results, results_withExpansion)
results_together$Scenario=factor(results_together$Scenario, levels=c("Current", "Expansion"))

order_inf<-read.csv("~/Desktop/Sarah/Salmon/CSAS_PropPressure/River_Order.csv", header=T)

results_together_with_order<- merge(results_together, order_inf, by="River")

barplot<- ggplot()+geom_bar(data=results_together_with_order, aes(y=prop.press, x=reorder(River, order), fill=Scenario), 
                  stat="identity", position="dodge")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2),
                   axis.title = element_text(size=12), axis.text.y = element_text(size=10))+
  ylab("Propagule Pressure")+xlab("River")+scale_fill_manual(values=c("dodgerblue", "firebrick2"))+
NULL

#Save plot as 12 x 18 overall 
map_CurrentSites /
  map_with_Expansion  /
  barplot

