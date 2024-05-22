library(sp)
library(raster)
library(terra)
library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(lubridate)

C1_652<-read.csv("C:/D1MPA_2024_Harmony/Krill_Fishery_C1_2022/652_CHL_2023-10-15/Data product/C1_652.csv")
# data requisition 652: c1 data from 1979 to nov 2022

C1_652$catch_start<-as.POSIXct(strptime(C1_652$date_catchperiod_start, 
                                     format="%Y-%m-%d", tz="GMT"))

C1_652$haul_start<-as.POSIXct(strptime(C1_652$datetime_haul_start, 
                                     format="%Y-%m-%d %H:%M:%S", tz="GMT"))

C1_652$haul_end<-as.POSIXct(strptime(C1_652$datetime_haul_end, 
                                          format="%Y-%m-%d %H:%M:%S", tz="GMT"))
summary(as.factor(C1_652$asd_code))

C1_652<-subset(C1_652,asd_code=="481"|
                 asd_code=="482"|
                 asd_code=="883")

catch2012<-subset(C1_652,catch_start>'2011-12-31',
                  select=c("c1_id","trawl_technique","asd_code",
                           "catch_start","haul_start","haul_end",
                           "depth_gear_haul_start_m","depth_bottom_haul_start_m",
                           "longitude_haul_start","latitude_haul_start",
                           "krill_greenweight_kg"))


names(catch2012)[names(catch2012) == "krill_greenweight_kg"] <-"greenweight_caught_kg"

# data requisition 669 C1 data between 2020 and 2023

c1<-readRDS("C:/D1MPA_2024_Harmony/Krill_Fishery_C1_2022/669_CHL_2024-03-20.Rds")

c1_catch<-c1$C1_CATCH

c1<-c1$C1

summary(as.factor(c1$target_species))
summary(as.factor(c1$catchperiod_code))
summary(as.factor(c1$asd_code))
c1<-subset(c1,asd_code!="483")
summary(as.factor(c1_catch$taxon_code))



kc<-subset(c1_catch, taxon_code=="KRI",select=c("c1_id","greenweight_caught_kg")) # Krill Catch

c1_kc <- merge(c1, kc, by="c1_id")

head(c1_kc)

c1_kc$catch_start<-as.POSIXct(strptime(c1_kc$date_catchperiod_start, 
                                        format="%Y-%m-%d", tz="GMT"))


c1_kc$haul_start<-as.POSIXct(strptime(c1_kc$datetime_haul_start, 
                                          format="%Y-%m-%d %H:%M:%S", tz="GMT"))

c1_kc$haul_end<-as.POSIXct(strptime(c1_kc$datetime_haul_end, 
                                        format="%Y-%m-%d %H:%M:%S", tz="GMT"))


# in the 625 there is data only up to 19 nov 2022, so the rest of november and december comes from 669

c1kc<-subset(c1_kc,haul_start>'2022-11-19 01:30:00.0000' & 
               haul_start<'2023-01-01 00:00:01.0000',
             select=c("c1_id","trawl_technique","asd_code",
                      "catch_start","haul_start","haul_end",
                      "depth_gear_haul_start_m","depth_bottom_haul_start_m",
                      "longitude_haul_start","latitude_haul_start",
                      "greenweight_caught_kg"))


### join data frames

catch<-rbind(catch2012,c1kc)

head(catch)

summary(as.factor(catch$asd_code))

# clean environemnt, except the data "catch" and then claen unused memory

#rm(list=setdiff(ls(), "catch"))

#gc()


# transform the catch data into a spatial object 

EPSG4326<-CRS("+proj=longlat +datum=WGS84 +no_defs")

EPSGlaea<-CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")


cpsp<-SpatialPointsDataFrame(catch[9:10],catch,proj4string = EPSG4326)

cpspM<-sp::spTransform(cpsp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

#shapefile(cpspM,"catch_2012_2022.shp")
cpspM$year<-year(cpspM$haul_start)

#catch_location<-plyr::ddply(data.frame(cpspM), c("year","coords.x1","coords.x2"), summarise,
#            depth_gear=mean(depth_gear_haul_start_m),
#            depth_bottom=mean(depth_bottom_haul_start_m),
#           catch_ton=sum(greenweight_caught_kg)/1000) 

median(na.omit(cpspM$depth_bottom_haul_start_m))

quantile(na.omit(cpspM$depth_bottom_haul_start_m))

quantile(na.omit(cpspM$depth_bottom_haul_start_m),probs=0.85)

ggplot(data.frame(cpspM),aes(depth_bottom_haul_start_m))+geom_histogram()+
  theme_bw()+xlab("Bottom depth on haul start (m)")+
  ylab("Number of hauls")+
  geom_vline(xintercept = 1000,linetype="dashed",colour="red",linewidth=1.4)+
  geom_vline(xintercept = 340,linetype="dotted",colour="blue",linewidth=1.4)+
  geom_vline(xintercept = 717,linetype="dotdash",colour="green",linewidth=1.4)


#write.csv(catch_location,"catch_per_year_coords.csv")

# load D1MPA new boundaries 

d1mpa<-shapefile("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/GPZ-SPZ/D1MPA_Vertices_2024.shp")

plot(d1mpa)

same.crs(d1mpa,cpspM)

catch_mpa<-raster::intersect(cpspM,d1mpa)

head(catch_mpa)


cmpa<-merge(catch,data.frame(catch_mpa),all=T)

head(cmpa)

summary(as.factor(cmpa$Name))
summary(as.factor(cmpa$Zone))

cmpa$Name[is.na(cmpa$Name)]<-"Outside MPA"

cmpa$Zone[is.na(cmpa$Zone)]<-"Outside MPA"

cmpa$year<-year(cmpa$haul_start)
cmpa$quarter<-quarter(cmpa$haul_start)
head(cmpa)

cmpa$seasonal[cmpa$Zone=="SPZ" & (cmpa$quarter=="1" | cmpa$quarter=="4")]<-TRUE

gpz.out<-subset(cmpa,Zone!="SPZ")
spz<-subset(cmpa,seasonal==T)
cmpaS<-rbind(gpz.out,spz)


mpaM<-plyr::ddply(cmpaS, c("year","Name","Zone"), summarise,
                   depth_gear=mean(depth_gear_haul_start_m),
                   depth_bottom=mean(depth_bottom_haul_start_m),
                   catch_ton=sum(greenweight_caught_kg)/1000) 

catchM<-plyr::ddply(cmpa, c("year"), summarise,
                   catch_ton_tot=sum(greenweight_caught_kg)/1000) 


cmpaM<-merge(mpaM,catchM)

head(cmpaM)

cmpaM$catch_perc<-(cmpaM$catch_ton/cmpaM$catch_ton_tot)*100 # percentage of cacth within the D1MPA
cmpaM$catch_prop<-(cmpaM$catch_ton/cmpaM$catch_ton_tot) # proportion of cacth within the D1MPA, so a binomial plot can be done

summary(cmpaM$catch_perc[cmpaM$Zone!="Outside MPA"])
sd(cmpaM$catch_perc[cmpaM$Zone!="Outside MPA"])/
  sqrt(length(cmpaM$catch_perc[cmpaM$Zone!="Outside MPA"])-1)



# total catch within D1MPA

d1mpadf<-subset(cmpaM,Zone!="Outside MPA")

y18on<-plyr::ddply(d1mpadf, c("year"), summarise,
                     proptot=sum(catch_prop)) 


ggplot(y18on,aes(year,proptot*100))+
  geom_smooth(se=F,method="gam",formula=y~s(x,k=6))+
  geom_point(size=3)+
  scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
  theme_bw()+xlab("Year")+ylab("Catch inside D1MPA (%)")


### -- Load VRZ shapefiles and do a similar calcualtion 

vrzs<-shapefile("C:/D1MPA_2024_Harmony/Krill_Fishery_C1_2022/VRZ/VRZall.shp")

summary(as.factor(vrzs$layer))

vrzl<-sp::spTransform(vrzs,EPSGlaea)

plot(vrzl)
head(vrzl)
same.crs(cpspM,vrzl)

catch_vrz<-raster::intersect(cpspM,vrzl)

head(catch_vrz)


cvrz<-merge(catch,data.frame(catch_vrz),all=T)

head(cvrz)

cvrz$year<-year(cvrz$haul_start)

cvrz$month<-month(cvrz$haul_start)

summary(as.factor(cvrz$layer))

vgn<-subset(cvrz,layer=="VRZ Gerlache North 30km" &(month>9 | month<3) )
vgs<-subset(cvrz,layer=="VRZ Gerlache South 30km"&(month>9 | month<3) )
vNAP<-subset(cvrz,layer=="VRZ NAP 40km"&(month>9 | month<3) )
vShet<-subset(cvrz,layer=="VRZ Shetlands 40km"&(month>10 | month<4) )


vrz<-rbind(vgn,vgs,vNAP,vShet)

head(vrz)

vrzM<-plyr::ddply(vrz, c("year","layer"), summarise,
                         depth_gear=mean(depth_gear_haul_start_m),
                         depth_bottom=mean(depth_bottom_haul_start_m),
                         catch_ton=sum(greenweight_caught_kg)/1000) 


vrzmm<-merge(vrzM,catchM)


vrzmm$catch_perc<-(vrzmm$catch_ton/vrzmm$catch_ton_tot)*100 # percentage of cacth within the D1MPA
vrzmm$catch_prop<-(vrzmm$catch_ton/vrzmm$catch_ton_tot) # proportion of cacth within the D1MPA, so a binomial plot can be done

vrzdf<-data.frame(year=vrzmm$year,Name=vrzmm$layer,Zone=c("VRZ"),
                  vrzmm[3:8])
head(cmpaM)

cmp.vrz<-rbind(cmpaM,vrzdf)

summary(as.factor(cmp.vrz$Name))

ggplot(subset(cmp.vrz,Name=="Outside MPA"),
        aes(year,catch_prop*100))+
    geom_smooth(se=F,method="gam",formula=y~s(x,k=3))+
    geom_point(size=3)+
    scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    ggtitle(label="a. Outside MPA")+
    
    ggplot(subset(cmp.vrz,Name=="SOI"),
           aes(year,catch_prop*100))+
    geom_smooth(se=F,method="gam",formula=y~s(x,k=8))+
    geom_point(size=3)+
    scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    ggtitle(label="b. GPZ South Orkneys")+
    
    ggplot(subset(cmp.vrz,Name=="EI"),
           aes(year,catch_prop*100))+
    geom_smooth(se=F,method="gam",formula=y~s(x,k=7))+
    geom_point(size=3)+
    scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    ggtitle(label="c. GPZ Elephant Island")+
    
    ggplot(subset(cmp.vrz,Name=="SSIW"),
           aes(year,catch_prop*100))+
    geom_smooth(se=F,method="gam",formula=y~s(x,k=9))+
    geom_point(size=3)+
    scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    theme_bw()+xlab("Year")+ylab("Catch (%)")+
    ggtitle(label="c. GPZ South Shetlands West")+
  
  ggplot(subset(cmp.vrz,Name=="NWAP"),
          aes(year,catch_prop*100))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=9))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="d. GPZ Northwest Antarctic Peninsula")+
     
     
     ggplot(subset(cmp.vrz,Name=="SSIE"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=9))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="e. SPZ South Shetlands East")+
     
     ggplot(subset(cmp.vrz,Name=="JOIN"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=2))+
     geom_point(size=3)+
  scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="f. SPZ Joinville")+
     
     ggplot(subset(cmp.vrz,Name=="SLI"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=6))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="g. SPZ Smith and Low Islands")+
  
  ggplot(subset(cmp.vrz,Name=="VRZ Shetlands 40km"),
          aes(year,catch_prop*100))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=9))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="h. VRZ Shetlands")+
     
     
     ggplot(subset(cmp.vrz,Name=="VRZ NAP 40km"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=2))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="i. VRZ NAP")+
     
     ggplot(subset(cmp.vrz,Name=="VRZ Gerlache North 30km"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=6))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="j. VRZ Gerlache North")+
     
     ggplot(subset(cmp.vrz,Name=="VRZ Gerlache South 30km"),
            aes(year,catch_prop))+
     geom_smooth(se=F,method="gam",formula=y~s(x,k=2))+
     geom_point(size=3)+
     scale_x_continuous(limits=c(2012,2022),breaks=c(2012,2014,2016,2018,2020,2022))+
     theme_bw()+xlab("Year")+ylab("Catch (%)")+
     ggtitle(label="k. VRZ Gerlache South 30km")
