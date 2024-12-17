## This is a script to produce plots of length at age data for the different surveys plus maps to check if the data come from a geographic area that is representative of the species distribution

# install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
library(DATRAS)
library(icesDatras)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mapdata)
library(sf)
library(sp)

load("Data/Surveys_nostrict.Rdata")

##### IBTS
datras <- NS_IBTS_nostrict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")

##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))


HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))

HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ3 <- CAdata[CAdata$Quarter==3,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_IBTS_Q1 <-c("Clupea harengus" ,"Gadus morhua" ,"Glyptocephalus cynoglossus","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt",  "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus" ,"Trisopterus esmarkii")


speciesQ3 <- as.data.frame(table(CAdataQ3$Year, factor(CAdataQ3$Species)))

speciesQ3<- pivot_wider(speciesQ3, names_from = "Var2", values_from = "Freq")

species_IBTS_Q3 <-c("Clupea harengus" ,"Gadus morhua" ,"Glyptocephalus cynoglossus","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt",  "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus" ,"Trisopterus esmarkii")

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1,]
CAdataQ3 <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3,]


### plot length at age per species and year
dir.create("L_At_Age")

for (i in 1:length(species_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_IBTS_Q1[i],"IBTS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_IBTS_Q1[i]," IBTS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_IBTS_Q3)) {
  tmp <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_IBTS_Q3[i],"IBTS Q3"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_IBTS_Q3[i]," IBTS Q3.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-6,13), ylim = c(50,63))
map1 <- st_as_sf(map1)


for (i in 1:length(species_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-6,15), ylim =c(48,63))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_IBTS_Q1[i],"IBTS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_IBTS_Q1[i]," IBTS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_IBTS_Q3)) {
  tmp <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3[i],]
  tmp <- HHdata[HHdata$Quarter == 3 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-6,15), ylim =c(48,63))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_IBTS_Q3[i],"IBTS Q3"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_IBTS_Q3[i]," IBTS Q3_map.png"), width=10, height=10, dpi=300)
} 


##### EVHOE
datras <- EVHOE_nostrict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_EVHOE_Q4 <-c("Chelidonichthys cuculus" ,"Gadus morhua" ,"Lepidorhombus whiffiagonis","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt", "Mullus surmuletus","Phycis blennoides", "Pleuronectes platessa","Solea solea")


### filter species with enough data

CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4,]


### plot length at age per species and year
dir.create("L_At_Age")

for (i in 1:length(species_EVHOE_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_EVHOE_Q4[i],"EVHOE Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_EVHOE_Q4[i]," EVHOE Q4.png"), width=10, height=10, dpi=300)
} 







map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-12,0), ylim = c(43,53))
map1 <- st_as_sf(map1)


for (i in 1:length(species_EVHOE_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-12,0), ylim =c(43,53))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_EVHOE_Q4[i],"EVHOE Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_EVHOE_Q4[i]," EVHOE Q4_map.png"), width=10, height=10, dpi=300)
} 



##### SWC IBTS

datras <- SWC_IBTS_nostrict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")

### remove hauls without lat or long

HHdata <- HHdata[!is.na(HHdata$lat) & !is.na(HHdata$lon),]


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_SWC_IBTS_Q1 <- names(speciesQ1)[2:9]


speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_SWC_IBTS_Q4 <-names(speciesQ4)[2:9]

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1,]
CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4,]


### plot length at age per species and year
dir.create("L_At_Age")

for (i in 1:length(species_SWC_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SWC_IBTS_Q1[i],"SWC IBTS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SWC_IBTS_Q1[i]," SWC IBTS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SWC_IBTS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SWC_IBTS_Q4[i],"SWC IBTS Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SWC_IBTS_Q4[i]," SWC IBTS Q4.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-11,-3), ylim = c(53,61))
map1 <- st_as_sf(map1)


for (i in 1:length(species_SWC_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SWC_IBTS_Q1[i],"SWC IBTS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SWC_IBTS_Q1[i]," SWC IBTS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SWC_IBTS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SWC_IBTS_Q4[i],"SWC IBTS Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SWC_IBTS_Q4[i]," SWC IBTS Q4_map.png"), width=10, height=10, dpi=300)
} 


##### SCOWCGFS
datras <- SCOWCGFS_nostrict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_SCOWCGFS_Q1 <- c("Clupea harengus" ,"Gadus morhua", "Melanogrammus aeglefinus","Merlangius merlangus"  ,"Pollachius virens", "Scomber scombrus" ,"Sprattus sprattus", "Trisopterus esmarkii" ) 


speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_SCOWCGFS_Q4 <-c("Clupea harengus" ,"Gadus morhua", "Melanogrammus aeglefinus","Merlangius merlangus"  ,"Pollachius virens", "Scomber scombrus" ,"Sprattus sprattus", "Trisopterus esmarkii" ) 

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1,]
CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4,]


### plot length at age per species and year
dir.create("L_At_Age")

for (i in 1:length(species_SCOWCGFS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SCOWCGFS_Q1[i],"SCOWCGFS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SCOWCGFS_Q1[i]," SCOWCGFS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SCOWCGFS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SCOWCGFS_Q4[i],"SCOWCGFS Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SCOWCGFS_Q4[i]," SCOWCGFS Q4.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-11,-3), ylim = c(53,61))
map1 <- st_as_sf(map1)


for (i in 1:length(species_SCOWCGFS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SCOWCGFS_Q1[i],"SCOWCGFS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SCOWCGFS_Q1[i]," SCOWCGFS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SCOWCGFS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SCOWCGFS_Q4[i],"SCOWCGFS Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age/",species_SCOWCGFS_Q4[i]," SCOWCGFS Q4_map.png"), width=10, height=10, dpi=300)
} 



###### STRICT #######

load("Data/Surveys_strict.Rdata")

##### IBTS
datras <- NS_IBTS

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ3 <- CAdata[CAdata$Quarter==3,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_IBTS_Q1 <-c("Clupea harengus" ,"Gadus morhua" ,"Glyptocephalus cynoglossus","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt",  "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus" ,"Trisopterus esmarkii")


speciesQ3 <- as.data.frame(table(CAdataQ3$Year, factor(CAdataQ3$Species)))

speciesQ3<- pivot_wider(speciesQ3, names_from = "Var2", values_from = "Freq")

species_IBTS_Q3 <-c("Clupea harengus" ,"Gadus morhua" ,"Glyptocephalus cynoglossus","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt",  "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus" ,"Trisopterus esmarkii")

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1,]
CAdataQ3 <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3,]


### plot length at age per species and year
dir.create("L_At_Age_strict")

for (i in 1:length(species_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_IBTS_Q1[i],"IBTS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_IBTS_Q1[i]," IBTS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_IBTS_Q3)) {
  tmp <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_IBTS_Q3[i],"IBTS Q3"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_IBTS_Q3[i]," IBTS Q3.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-6,13), ylim = c(50,63))
map1 <- st_as_sf(map1)


for (i in 1:length(species_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_IBTS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-6,15), ylim =c(48,63))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_IBTS_Q1[i],"IBTS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_IBTS_Q1[i]," IBTS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_IBTS_Q3)) {
  tmp <- CAdataQ3[CAdataQ3$Species %in% species_IBTS_Q3[i],]
  tmp <- HHdata[HHdata$Quarter == 3 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-6,15), ylim =c(48,63))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_IBTS_Q3[i],"IBTS Q3"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_IBTS_Q3[i]," IBTS Q3_map.png"), width=10, height=10, dpi=300)
} 


##### EVHOE
datras <- EVHOE_strict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_EVHOE_Q4 <-c("Chelidonichthys cuculus" ,"Gadus morhua" ,"Lepidorhombus whiffiagonis","Melanogrammus aeglefinus" ,  "Merlangius merlangus","Microstomus kitt", "Mullus surmuletus","Phycis blennoides", "Pleuronectes platessa","Solea solea")


### filter species with enough data

CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4,]


### plot length at age per species and year
dir.create("L_At_Age_strict")

for (i in 1:length(species_EVHOE_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_EVHOE_Q4[i],"EVHOE Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_EVHOE_Q4[i]," EVHOE Q4.png"), width=10, height=10, dpi=300)
} 







map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-12,0), ylim = c(43,53))
map1 <- st_as_sf(map1)


for (i in 1:length(species_EVHOE_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_EVHOE_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-12,0), ylim =c(43,53))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_EVHOE_Q4[i],"EVHOE Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_EVHOE_Q4[i]," EVHOE Q4_map.png"), width=10, height=10, dpi=300)
} 



##### SWC IBTS

datras <- SWC_IBTS_strict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")

### remove hauls without lat or long

HHdata <- HHdata[!is.na(HHdata$lat) & !is.na(HHdata$lon),]


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_SWC_IBTS_Q1 <- names(speciesQ1)[2:9]


speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_SWC_IBTS_Q4 <-names(speciesQ4)[2:9]

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1,]
CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4,]


### plot length at age per species and year
dir.create("L_At_Age_strict")

for (i in 1:length(species_SWC_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SWC_IBTS_Q1[i],"SWC IBTS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SWC_IBTS_Q1[i]," SWC IBTS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SWC_IBTS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SWC_IBTS_Q4[i],"SWC IBTS Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SWC_IBTS_Q4[i]," SWC IBTS Q4.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-11,-3), ylim = c(53,61))
map1 <- st_as_sf(map1)


for (i in 1:length(species_SWC_IBTS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SWC_IBTS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SWC_IBTS_Q1[i],"SWC IBTS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SWC_IBTS_Q1[i]," SWC IBTS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SWC_IBTS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SWC_IBTS_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SWC_IBTS_Q4[i],"SWC IBTS Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SWC_IBTS_Q4[i]," SWC IBTS Q4_map.png"), width=10, height=10, dpi=300)
} 


##### SCOWCGFS
datras <- SCOWCGFS_strict

####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country 

HHdata <- HH %>%
  filter(HaulVal %in% c("A","N","V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- st_read("ICES_rect_shapefile/ices_squares_simple.shp")


##### match ICES rect
id <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(xx, 'Spatial'))

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- st_read("ICES_areas/ices_areas.shp")


idindex <- over(SpatialPoints(HHdata[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")), as(yy, 'Spatial'))
HHdata$SD <- idindex$ICES_area

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)


####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id,]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"SD"] 
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"Rect"] 
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"HaulVal"] 
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"StdSpecRecCode"] 
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id),"BycSpecRecCode"] 


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id,]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"SD"] 
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"Rect"] 
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id),"HaulVal"] 


### get only records with both length and age

CAdata <- CAdata[!is.na(CAdata$Age) & !is.na(CAdata$LngtCm),]

CAdataQ1 <- CAdata[CAdata$Quarter==1,]
CAdataQ4 <- CAdata[CAdata$Quarter==4,]

speciesQ1 <- as.data.frame(table(CAdataQ1$Year, factor(CAdataQ1$Species)))

speciesQ1<- pivot_wider(speciesQ1, names_from = "Var2", values_from = "Freq")

species_SCOWCGFS_Q1 <- c("Clupea harengus" ,"Gadus morhua", "Melanogrammus aeglefinus","Merlangius merlangus"  ,"Pollachius virens", "Scomber scombrus" ,"Sprattus sprattus", "Trisopterus esmarkii" ) 


speciesQ4 <- as.data.frame(table(CAdataQ4$Year, factor(CAdataQ4$Species)))

speciesQ4<- pivot_wider(speciesQ4, names_from = "Var2", values_from = "Freq")

species_SCOWCGFS_Q4 <-c("Clupea harengus" ,"Gadus morhua", "Melanogrammus aeglefinus","Merlangius merlangus"  ,"Pollachius virens", "Scomber scombrus" ,"Sprattus sprattus", "Trisopterus esmarkii" ) 

### filter species with enough data

CAdataQ1 <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1,]
CAdataQ4 <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4,]


### plot length at age per species and year
dir.create("L_At_Age_strict")

for (i in 1:length(species_SCOWCGFS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SCOWCGFS_Q1[i],"SCOWCGFS Q1"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SCOWCGFS_Q1[i]," SCOWCGFS Q1.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SCOWCGFS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4[i],]
  
  ggplot(tmp, aes(Age,LngtCm))+geom_point()+facet_wrap(~Year)+ggtitle(paste(species_SCOWCGFS_Q4[i],"SCOWCGFS Q4"))
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SCOWCGFS_Q4[i]," SCOWCGFS Q4.png"), width=10, height=10, dpi=300)
} 




map1 <- map("worldHires", fill = TRUE, plot = FALSE,xlim = c(-11,-3), ylim = c(53,61))
map1 <- st_as_sf(map1)


for (i in 1:length(species_SCOWCGFS_Q1)) {
  tmp <- CAdataQ1[CAdataQ1$Species %in% species_SCOWCGFS_Q1[i],]
  tmp <- HHdata[HHdata$Quarter == 1 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SCOWCGFS_Q1[i],"SCOWCGFS Q1"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SCOWCGFS_Q1[i]," SCOWCGFS Q1_map.png"), width=10, height=10, dpi=300)
} 


for (i in 1:length(species_SCOWCGFS_Q4)) {
  tmp <- CAdataQ4[CAdataQ4$Species %in% species_SCOWCGFS_Q4[i],]
  tmp <- HHdata[HHdata$Quarter == 4 & HHdata$haul.id %in% unique(tmp$haul.id),]
  
  ggplot(tmp)+ geom_sf(data=map1)+geom_point(aes(x=as.numeric(ShootLong), y=as.numeric(ShootLat)),color="red", alpha=0.5, size=1)+coord_sf(xlim = c(-11,-3), ylim =c(53,61))+xlab("Longitude")+ylab("Latitude")+ggtitle(paste(species_SCOWCGFS_Q4[i],"SCOWCGFS Q4"))+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ facet_wrap(~Year)
  
  
  ggsave(last_plot(), file=paste0("L_At_Age_strict/",species_SCOWCGFS_Q4[i]," SCOWCGFS Q4_map.png"), width=10, height=10, dpi=300)
} 




