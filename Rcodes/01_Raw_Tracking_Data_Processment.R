
library(lubridate)
library(raster)
library(dplyr)
library(adehabitatHR)
library(adehabitatLT)
library(sp)
library(trip)
library(ggplot2)
library(patchwork)
library(terra)
library(reshape2)


### Load raster mask and generate spatial coordinate system objects
### load raster mask and generate spatial coordinate system objects
mask <- raster("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/D1_Mask/D1_Mask.tif") # mask

plot(mask)

EPSG6932<-crs(mask) # extract the coordinate spatial system from the object

EPSG4326<-CRS("+proj=longlat +datum=WGS84 +no_defs")

D1<-shapefile("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Shapefiles/D1_6932.shp")

###------Humpback whale----------------

hump<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/humpback_whales/fastOutTracks_humpback.csv")
hump2<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/humpback_whales/tracks.csv")

hump2df<-data.frame(id=as.factor(hump2$individual_id),
                    date=hump2$date,
                    lc=hump2$location_quality,
                    lon=hump2$decimal_longitude,
                    lat=hump2$decimal_latitude)

humps<-rbind(hump,hump2df)

humps$timestamp<-as.POSIXct(strptime(humps$date, 
                                     format="%Y-%m-%d %H:%M:%S", tz="GMT"))

humps <- humps[!duplicated(humps[, c("lon", "lat","timestamp","id")]), ]

humpd<-subset(humps,lon<0 & lon>(-150))

humpd$month<-month(humpd$timestamp)

humpd<-na.omit(humpd)

humpsp<-SpatialPointsDataFrame(humpd[4:5],humpd,proj4string = EPSG4326)

hbwspW<-sp::spTransform(humpsp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

hbwspW<-raster::intersect(hbwspW,D1)

#plot(hbwspW)

hbw<-data.frame(hbwspW)

hbw$id<-factor(hbw$id)

hbw<- hbw[!duplicated(hbw[, c("timestamp","id")]), ]

hbw.lt<-as.ltraj(xy=hbw[15:16],date=hbw$timestamp,id=hbw$id,proj4string = EPSG6932)

hbw.ld<-ld(hbw.lt)

ggplot(hbw.ld,aes(dist))+geom_histogram()

hbw.ld2<-subset(hbw.ld,dist<50000) #eliminate positions based on consecutive points distance 

length(unique(hbw.ld2$id))
length((hbw.ld2$id))
summary(as.factor(year(hbw.ld2$date)))
summary(as.factor(month(hbw.ld2$date)))

write.csv(hbw.ld2,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_Tracking_Data/Whale_Humpback.csv")

### -------------- minke whale----------------

minke<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data//minke_whales/fastOutTracks_minke.csv")

minke$timestamp<-as.POSIXct(strptime(minke$date, 
                                     format="%Y-%m-%d %H:%M:%S", tz="GMT"))

minkes <- minke[!duplicated(minke[, c("lon", "lat","timestamp","id")]), ]

minked<-subset(minkes,lon<0 & lon>(-150))

minked$month<-month(minked$timestamp)

minked<-na.omit(minked)

minkem<-data.frame(minked$month)

minkesp<-SpatialPointsDataFrame(minked[4:5],minked,proj4string = EPSG4326)

minkespW<-sp::spTransform(minkesp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
minkespW<-raster::intersect(minkespW,D1)

amw<-data.frame(minkespW)

amw$id<-factor(amw$id)

amw<- amw[!duplicated(amw[, c("timestamp","id")]), ]

amw.lt<-as.ltraj(xy=amw[15:16],date=amw$timestamp,id=amw$id,proj4string = EPSG6932)

amw.ld<-ld(amw.lt)

ggplot(amw.ld,aes(dist))+geom_histogram()

amw.ld2<-subset(amw.ld,dist<50000) #eliminate positions based on consecutive points distance 

amw.ld2$quarter<-quarter(amw.ld2$date)

amw.ld2$idy<-paste(amw.ld2$id,year(amw.ld2$date),month(amw.ld2$date),week(amw.ld2$date))

write.csv(amw.ld2,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_tracking_Data/Whale_AntarcticMinke.csv")

### ---------- Crabeater Seal----------------

#RAATD

crab<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/SCAR_EGBAMM_RAATD_2018_Standardised/RAATD_CRAS_standardized.csv")


crab$timestamp<-as.POSIXct(strptime(paste(paste(crab$year,crab$month,crab$day,sep="-"),
                                          crab$time), 
                                    format="%Y-%m-%d %H:%M:%S", tz="GMT"))

crab<-subset(crab,location_quality!="Z")

crabd<-data.frame(Lon=crab$decimal_longitude,
                  Lat=crab$decimal_latitude,
                  id=as.factor(crab$individual_id),Spp=crab$abbreviated_name,
                  timestamp=crab$timestamp)

crabd<-subset(crabd,Lon<0 & Lon>(-150))

crabd$month<-month(crabd$timestamp)

crabd<-na.omit(crabd)

crabm<-data.frame(crabd$month)

crabsp<-SpatialPointsDataFrame(crabd[1:2],crabd,proj4string = EPSG4326)

crabspW<-sp::spTransform(crabsp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

crabspW<-raster::intersect(crabspW,D1)

#plot(crabspW)

crab<-data.frame(crabspW)

crab$id<-factor(crab$id)

crab<- crab[!duplicated(crab[, c("timestamp","id")]), ]

crab.lt<-as.ltraj(xy=crab[14:15],date=crab$timestamp,id=crab$id,proj4string = EPSG6932)

crab.ld<-ld(crab.lt)

ggplot(crab.ld,aes(dist))+geom_histogram()

crab.ld2<-subset(crab.ld,dist<50000) #eliminate positions based on consecutive points distance 

crab.ld2$quarter<-quarter(crab.ld2$date)
crab.ld2$idy<-paste(crab.ld2$id,year(crab.ld2$date),month=month(crab.ld2$date))


write.csv(crab.ld2,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_Tracking_Data/Seal_Crabeater.csv")

### ---------Fur Seal---------------
# Hinke et al. 2017

jh17<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/Pygoscelis_and_furseal_HInke_etal_2017_pone.0170132.s001/Data/satellite telemetry.csv")

head(jh17)

summary(as.factor(jh17$Loc.Qual))

jh17<-subset(jh17,Loc.Qual!="Z")
jh17<-subset(jh17,Loc.Qual!="0")
#jh17<-subset(jh17,Loc.Qual!="")

jh17$timestamp<-as.POSIXct(strptime(paste(jh17$Date,jh17$Time), format="%m/%d/%Y %H:%M:%S", tz="GMT"))

jh17<-data.frame(Lon=jh17$Longitude,Lat=jh17$Latitude,id=as.factor(jh17$Deployment),
                 Spp=jh17$Spp,timestamp=jh17$timestamp,Col=jh17$Site)
head(jh17)

summary(as.factor(jh17$Spp))

jhf<-subset(jh17,Spp=="AFS")

head(jhf)
summary(jhf$timestamp)

jhd<-jhf


# RAATD

ra<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/SCAR_EGBAMM_RAATD_2018_Standardised/RAATD_ANFS_standardized.csv")


ra$timestamp<-as.POSIXct(strptime(paste(paste(ra$year,ra$month,ra$day,sep="-"),
                                        ra$time), 
                                  format="%Y-%m-%d %H:%M:%S", tz="GMT"))
head(ra)

summary(as.factor(ra$location_quality))

ra<-subset(ra,location_quality!="Z")


raat<-data.frame(Lon=ra$decimal_longitude,
                 Lat=ra$decimal_latitude,
                 id=as.factor(ra$individual_id),Spp=ra$abbreviated_name,
                 timestamp=ra$timestamp,Col=c("SOI"))

raatd<-subset(raat,Lon<0 & Lon>(-150))

head(raatd)

raatd$Spp<-c("AFS")

###Krause et al. 2022

kr<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/Antarctic_furseal_Krause_etal/AFSPOW_postapril.csv")

kr$timestamp<-as.POSIXct(strptime(kr$Date, 
                                  format="%m/%d/%Y %H:%M", tz="GMT"))

krd<-data.frame(Lon=kr$Longitude,
                Lat=kr$Latitude,
                id=as.factor(kr$Tag),Spp=kr$Spp,
                timestamp=kr$timestamp,Col=c("CS"))

afs<-rbind(jhd,raatd,krd)

afs<-na.omit(afs)

afs$id<-as.factor(afs$id)

afs$month<-month(afs$timestamp)

afs$pres<-c(1)

summary(as.factor(afs$Col))

afsp<-SpatialPointsDataFrame(afs[1:2],afs,proj4string = EPSG4326)

afspW<-sp::spTransform(afsp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

afspW<-raster::intersect(afspW,D1)

#plot(afspW)

afs<-data.frame(afspW)

afs$id<-factor(afs$id)

afs<- afs[!duplicated(afs[, c("timestamp","id")]), ]

afs.lt<-as.ltraj(xy=afs[16:17],date=afs$timestamp,id=afs$id,proj4string = EPSG6932)

afs.ld<-ld(afs.lt)

afs.ld2<-subset(afs.ld,dist<50000) #eliminate positions based on consecutive points distance 

summary(as.factor(year(afs.ld2$date)))

afs.ld2$year<-year(afs.ld2$date)

afs.ld2$IDy<-paste(afs.ld2$id,year(afs.ld2$date),month(afs.ld2$date))

write.csv(afs.ld2,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_Tracking_Data/Seal_Fur.csv")

###----------Elephant Seal------------


soes<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/SCAR_EGBAMM_RAATD_2018_Standardised/RAATD_SOES_standardized.csv")


soes$timestamp<-as.POSIXct(strptime(paste(paste(soes$year,soes$month,soes$day,sep="-"),
                                          soes$time), 
                                    format="%Y-%m-%d %H:%M:%S", tz="GMT"))
soes<-subset(soes,location_quality!="Z")

soesd<-data.frame(Lon=soes$decimal_longitude,
                  Lat=soes$decimal_latitude,
                  id=as.factor(soes$individual_id),Spp=soes$abbreviated_name,
                  timestamp=soes$timestamp)

soesd<-subset(soesd,Lon<0 & Lon>(-150))

soesd$month<-month(soesd$timestamp)

soesd<-na.omit(soesd)

soessp<-SpatialPointsDataFrame(soesd[1:2],soesd,proj4string = EPSG4326)

soesspW<-sp::spTransform(soessp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

soesspW<-raster::intersect(soesspW,D1)

soes2<-data.frame(soesspW)

soes2$id<-factor(soes2$id)

soes2<- soes2[!duplicated(soes2[, c("timestamp","id")]), ]

soes.lt<-as.ltraj(xy=soes2[14:15],date=soes2$timestamp,id=soes2$id,proj4string = EPSG6932)

soes.ld<-ld(soes.lt)

#ggplot(soes.ld2,aes(dist))+geom_histogram()

soes.ld2<-subset(soes.ld,dist<30000) #eliminate positions based on consecutive points distance 

soes.ld2$idy<-paste(soes.ld2$id,year(soes.ld2$date),week(soes.ld2$date))

write.csv(soes.ld2,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_Tracking_Data/Seal_Elephant.csv")


####-----------Penguins---------------


# Chinstrap, Adelie and Gentoo penguin data from two sources: 

#Hinke et al. 2017 https://doi.org/10.1371/journal.pone.0170132
# Hinke et al. 2020 https://doi.org/10.1098/rsbl.2020.0645

# Adelie Penguin: RAATD https://zenodo.org/record/3722948#.YqG-DpBBw-Q

#Gentoo Penguins: Korczak-Abshire et al. 2021 https://doi.org/10.1098/rsbl.2020.0708

#Lload and process data from each paper

# ----------Hinke et al. 2017-----------

jh17<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/Pygoscelis_and_furseal_HInke_etal_2017_pone.0170132.s001/Data/satellite telemetry.csv")

head(jh17)

#summary(as.factor(jh17$Loc.Qual))

jh17<-subset(jh17,Loc.Qual!="Z")
jh17<-subset(jh17,Loc.Qual!="0")
#jh17<-subset(jh17,Loc.Qual!="")

jh17$timestamp<-as.POSIXct(strptime(paste(jh17$Date,jh17$Time), format="%m/%d/%Y %H:%M:%S", tz="GMT"))

jh17<-data.frame(Lon=jh17$Longitude,Lat=jh17$Latitude,id=as.factor(jh17$Deployment),
                 Spp=jh17$Spp,timestamp=jh17$timestamp,Col=jh17$Site)

# ---------Hinke et al. 2020-----------

jh20<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/Pygoscelis_spp_Hinke_etal_bottleneck_paper/tracks.csv")

jh20$timestamp<-as.POSIXct(strptime(jh20$LOC_DATE, 
                                    format="%m/%d/%Y %H:%M:%S", tz="GMT"))

jh20<-data.frame(Lon=jh20$LONGITUDE,Lat=jh20$LATITUDE,id=as.factor(jh20$PTT),
                 Spp=jh20$SPP,timestamp=jh20$timestamp,Col=c("COPA"))

jh20$Spp[jh20$Spp=="PYD"]<-"ADPE"
jh20$Spp[jh20$Spp=="PYN"]<-"CHPE"
jh20$Spp[jh20$Spp=="PYP"]<-"GEPE"

###---- Korczak-Abshire et al. 2021--------

ka21<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/Gentoo_Penguin_Korczak-abshire_etal/manuscript_data.csv")

ka21$timestamp<-as.POSIXct(strptime(ka21$LOC_DATE, 
                                    format="%m/%d/%Y %H:%M", tz="GMT"))

ka21<-data.frame(Lon=ka21$LONGITUDE,Lat=ka21$LATITUDE,id=as.factor(ka21$PTT),Spp=c("GEPE"),
                 timestamp=ka21$timestamp,Col=ka21$COLONY)

### -----RAATD--------

ra<-read.csv("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Raw_Tracking_Data/SCAR_EGBAMM_RAATD_2018_Standardised/RAATD_ADPE_standardized.csv")


ra$timestamp<-as.POSIXct(strptime(paste(paste(ra$year,ra$month,ra$day,sep="-"),
                                        ra$time), 
                                  format="%Y-%m-%d %H:%M:%S", tz="GMT"))

ra<-subset(ra,location_quality!="Z")

raat<-data.frame(Lon=ra$decimal_longitude,
                 Lat=ra$decimal_latitude,
                 id=as.factor(ra$individual_id),Spp=ra$abbreviated_name,
                 timestamp=ra$timestamp,Col=c("SOI"))

raatd<-subset(raat,Lon<0 & Lon>(-150))

df5<-rbind(jh17,jh20,ka21,raatd)

pengs<-subset(df5,Spp=="ADPE"|Spp=="CHPE"|Spp=="GEPE")

head(pengs)

peng<- pengs[!duplicated(pengs[, c("Lat", "Lon","timestamp","id")]), ]

pengm<-plyr::ddply(pengs, c("id","Spp","timestamp","Col"), summarise,
                   lon=mean(Lon),
                   lat=mean(Lat)) 

pengN<-plyr::ddply(pengs, c("id","Spp"), summarise,
                   N=length(Lon)) 

pengm<-merge(pengm,pengN)

pengm<-subset(pengm,lon<0)
pengm<-subset(pengm,lat<(-50))
pengm<-subset(pengm,N>5)


write.csv(pengm,"C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Processed_Tracking_Data/penguins.csv") 


