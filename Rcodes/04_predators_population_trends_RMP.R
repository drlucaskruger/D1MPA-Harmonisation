
# load strata

strata<-shapefile("C:/D1MPA trimestral/ASAM2022 updated strata/Strata_6932.shp")

plot(strata)

###------------ penguins--------------

### data processing 
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)

#plots
library(ggplot2)
library(patchwork)
library(sjPlot)
library(kableExtra)
library(ggthemes)
library(ggpubr)

#models

library(lmerTest)
library(fitdistrplus)
library(AER)
library(easystats)


all.counts<-read.csv("C:/D1MPA trimestral/AllCounts_V_4_1.csv")

# first subset data within strata
head(all.counts)

pcounts<-SpatialPointsDataFrame(all.counts[4:5],all.counts[1:14],proj4string = EPSG4326)

pcounts_6932<-sp::spTransform(pcounts,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

pcounts.strata<-raster::intersect(pcounts_6932,strata)


pcs<-data.frame(pcounts.strata)
head(pcs)

nests<-subset(pcs,count_type=="nests")
summary(as.factor(subset(nests,ID=="SSIW")$site_id))# only cape shirreff has enough data in SSIW strata


### some colonies had multiple counts oiver the same season
### this summarises the count with the maximum nests


nests$longitude_epsg_4326

countsN<-ddply(nests, c("site_id","common_name","longitude_epsg_4326","latitude_epsg_4326","ID"), summarise,
               ncounts=length(penguin_count),
               interval=(max(season_starting)-min(season_starting)))

nestM<-merge(nests,countsN) # identify number of counts for each population by merging

head(nestM)
poisson.mtest(nestM$penguin_count[nestM$ncounts>5 & nestM$penguin_count>0],R=199)  ##test for poisson distribution

nestm3<-subset(nestM,ncounts>5 & penguin_count>0) 


# chinstrap penguin 

chp<-subset(nestm3,common_name=="chinstrap penguin")

summary(as.factor(subset(nestm3,site_id=="PING")$site_name))

summary(as.factor(chp$ID))

head(chp)

chp<-subset(chp, season_starting>1979,select=c("site_id","longitude_epsg_4326","latitude_epsg_4326",
                                               "ID","season_starting","penguin_count","accuracy"))
summary(as.factor(chp$site_id))

summary(subset(chp,site_id=="PCHA" |site_id=="WATE")$penguin_count)
summary(subset(chp,site_id=="SELV" )$penguin_count)
summary(subset(chp,site_id=="USEF" )$penguin_count)
summary(as.factor(subset(chp,site_id!="PING")$accuracy))
summary(as.factor(chp$accuracy))

chp<-subset(chp,site_id!="CUVE" & site_id!="PETE") # only 2 counts
chp<-subset(chp,site_id!="PCHA" & site_id!="WATE") # very small colonies (less than 100 nests)

ping<-subset(chp,site_id=="PING" & season_starting>2000)

ggplot(ping,aes(season_starting,penguin_count))+
  geom_smooth(method="gam",formula=y~s(x,k=3),se=F)+
  geom_point()+
  theme_bw()+xlab("breeding seasons")+ylab("penguin counts")+
  ggtitle(label="Pinguino island chinstrap penguins")

chp<-subset(chp,site_id!="PING") # this population is the only with substantial increases

chp.ac<-subset(chp,accuracy<3)

summary(as.factor(subset(chp,ID=="EI")$site_id))


### lets try a simple GLMM


chp.ss<-subset(chp.ac,ID=="SSIW")
chp.ei<-subset(chp.ac,ID=="EI")
chp.bs<-subset(chp.ac,ID=="BS")
chp.gs<-subset(chp.ac,ID=="GS")


chpm.ss<-glm(penguin_count~scale(season_starting),family="poisson",data=chp.ss)
summary(chpm.ss)

chpm.ei<-glm(penguin_count~scale(season_starting),family="poisson",data=chp.ei)
summary(chpm.ei)


chp.mmbs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=chp.bs) 
chp.mbs<-glm(penguin_count~scale(season_starting),family="poisson",data=chp.bs)

summary(chp.mmbs)
anova(chp.mmbs,chp.mbs) # significant random effect

chp.mmgs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=chp.gs) 
chp.mgs<-glm(penguin_count~scale(season_starting),family="poisson",data=chp.gs)

summary(chp.mmgs)
anova(chp.mmgs,chp.mgs)# significant random effect


### gentoo penguins 
gep<-subset(nestm3,common_name=="gentoo penguin")
gep<-subset(gep, season_starting>1979,select=c("site_id","longitude_epsg_4326","latitude_epsg_4326",
                                               "ID","season_starting","penguin_count","accuracy"))
summary(as.factor(gep$site_id))
gep.ac<-subset(gep,accuracy<3)


gep.ss<-subset(gep.ac,ID=="SSIW")
gep.ei<-subset(gep.ac,ID=="EI")
gep.bs<-subset(gep.ac,ID=="BS")
gep.gs<-subset(gep.ac,ID=="GS")


summary(as.factor(gep.ss$site_id))

gepm.ss<-glm(penguin_count~scale(season_starting),family="poisson",data=gep.ss)
summary(gepm.ss)
gepm.ei<-glm(penguin_count~scale(season_starting),family="poisson",data=gep.ei)
summary(gepm.ei)


gep.mmbs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=gep.bs) 
gep.mbs<-glm(penguin_count~scale(season_starting),family="poisson",data=gep.bs)

summary(gep.mmbs)
anova(gep.mmbs,gep.mbs)# significant random effect

gep.mmgs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=gep.gs) 
gep.mgs<-glm(penguin_count~scale(season_starting),family="poisson",data=gep.gs)

summary(gep.mmgs)
anova(gep.mmgs,gep.mgs)# significant random effect



###----adelie penguin ---------



adp<-subset(nestm3,common_name=="adelie penguin")
adp<-subset(adp, season_starting>1979,select=c("site_id","longitude_epsg_4326","latitude_epsg_4326",
                                               "ID","season_starting","penguin_count","accuracy"))
summary(as.factor(adp$site_id))

adp.ac<-subset(adp,accuracy<3)
adp.bs<-subset(adp.ac,ID=="BS")
adp.gs<-subset(adp.ac,ID=="GS")


adp.mmbs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=adp.bs) 
adp.mbs<-glm(penguin_count~scale(season_starting),family="poisson",data=adp.bs)

summary(adp.mmbs)
anova(adp.mmbs,adp.mbs) # significant random effect

adp.mmgs<-glmer(penguin_count~scale(season_starting)+(scale(season_starting)|site_id),family="poisson",data=adp.gs) 
adp.mgs<-glm(penguin_count~scale(season_starting),family="poisson",data=adp.gs)

summary(adp.mmgs)
anova(adp.mmgs,adp.mgs) # significant random effect


###  plots 

#elephant island 
chp.ei$Periods<-ifelse(chp.ei$season_starting<2000,"1985-1991","2009-2019")
gep.ei$Periods<-ifelse(gep.ei$season_starting<2000,"1985-1991","2009-2013")

ggplot(chp.ei,aes(Periods,penguin_count))+geom_boxplot()+
  theme_bw()+xlab("breeding seasons")+ylab("penguin counts")+
  ggtitle(label="a. Stinker Point chinstrap penguins")+

ggplot(gep.ei,aes(Periods,penguin_count))+geom_boxplot()+
  theme_bw()+xlab("breeding seasons")+ylab("penguin counts")+
  ggtitle(label="b. Stinker Point gentoo penguins")+

ggplot(chp.ss,aes(season_starting,penguin_count))+
  geom_smooth(method="gam")+
  geom_point()+
  theme_bw()+xlab("breeding seasons")+ylab("penguin counts")+
  ggtitle(label="c. Cape Shirreff chinstrap penguins")+


ggplot(gep.ss,aes(season_starting,penguin_count))+
  geom_smooth(method="gam")+
  geom_point()+
  theme_bw()+xlab("breeding seasons")+ylab("penguin counts")+
  ggtitle(label="d. Cape Shirreff gentoo penguins")



  (plot_model(chp.mmbs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.3,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="a. Chinstrap penguin (BS)")+
  
  plot_model(chp.mmgs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.3,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="b. Chinstrap penguin (GS)"))/


  
  (plot_model(adp.mmbs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.3,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="c. Adelie penguin (BS)")+
  
  plot_model(adp.mmgs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.3,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="d. adelie penguin (GS)"))/
  
  
  (plot_model(gep.mmbs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.3,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="e. Gentoo penguin (BS)")+
  
  plot_model(gep.mmgs,type="emm",terms=c("season_starting"),pred.type = "re",ci.lvl=0.75,
             show.data=T,se=T,transform="exp",grid=T,vcov.fun = std)+
  theme_bw()+xlab("breeding season")+ylab("number of nests")+
  ggtitle(label="f. Gentoo penguin (GS)"))

  
  # random effect plot
  
  plot_model(chp.mmbs,type="re",grid=F,sort.est="sort.all")[1]
  
#### --------- models diagnostics--------------


plot_model(chp.mmbs,type="diag")
plot_model(chp.mmgs,type="diag")
plot_model(adp.mmbs,type="diag")
plot_model(adp.mmgs,type="diag")

check_outliers(chp.mmbs)
check_outliers(chp.mmgs)
check_outliers(adp.mmbs)
check_outliers(adp.mmgs)
check_outliers(gep.mmbs)
check_outliers(gep.mmgs)



#### random effects

# in a GLMM the slope random effect is the differnce from the mean slope
# so lets calcualte the difference to know whether the tendecny of the pop is decreasing r incresing

chp.bs.re<-data.frame(ranef(chp.mmbs),stratum=c("BS"),species=c("CHP"))

summary(chp.mmbs)
chp.bs.re$trend<-chp.bs.re$condval-1.05

chp.gs.re<-data.frame(ranef(chp.mmgs),stratum=c("GS"),species=c("CHP"))
summary(chp.mmgs)
chp.gs.re$trend<-chp.gs.re$condval-0.057


adp.bs.re<-data.frame(ranef(adp.mmbs),stratum=c("BS"),species=c("ADP"))
summary(adp.mmbs)
adp.bs.re$trend<-adp.bs.re$condval-0.82


adp.gs.re<-data.frame(ranef(adp.mmgs),stratum=c("GS"),species=c("ADP"))
summary(adp.mmgs)
adp.gs.re$trend<-adp.gs.re$condval-0.553

gep.bs.re<-data.frame(ranef(gep.mmbs),stratum=c("BS"),species=c("GEP"))
summary(gep.mmbs)
gep.bs.re$trend<-gep.bs.re$condval+0.213

gep.gs.re<-data.frame(ranef(gep.mmgs),stratum=c("GS"),species=c("GEP"))
summary(gep.mmgs)
gep.gs.re$trend<-gep.gs.re$condval+0.3665


re<-rbind(chp.bs.re,chp.gs.re,
          adp.bs.re,adp.gs.re,
          gep.bs.re,gep.gs.re)



re<-subset(re,term=="scale(season_starting)")
redf<-data.frame(site_id=re$grp,slope=re$trend,sd=re$condsd,stratum=re$stratum,species=re$species)


coords<-ddply(nests, c("site_id"), summarise,
                       lon=mean(longitude_epsg_4326),
                       lat=mean(latitude_epsg_4326))

ranef<-merge(redf,coords)


# in a GLMM the slope random effect is the differnce from the mean slope

ggplot(subset(ranef,species=="CHP"),aes(reorder(site_id,+slope),slope,colour=stratum,shapefile=stratum))+
  geom_errorbar(aes(ymin=slope-sd,ymax=slope+sd))+
  geom_point()+coord_flip()


ggplot(ranef,aes(species,slope,fill=stratum,shape=stratum,linetype=stratum))+
  geom_boxplot()+coord_flip()+
  theme_bw()+ylab("std. random slope")+geom_hline(yintercept=0,linetype="dotted",linewidth=1.4)



site_coords<-ddply(nestm3, c("site_id","common_name"), summarise,
              lon=mean(longitude_epsg_4326),
              lat=mean(latitude_epsg_4326))
write.csv(site_coords,"C:/D1MPA trimestral/penguin_sites_coords.csv")
write.csv(ranef,"C:/D1MPA trimestral/ranef.csv")

### emperor penguin

head(all.counts)

emp<-subset(all.counts, common_name=="emperor penguin",select=c("site_id","longitude_epsg_4326","latitude_epsg_4326",
                                               "season_starting","penguin_count","accuracy"))


summary(as.factor(emp$site_id))

wap<-subset(emp,site_id=="ROTS"|site_id=="VERD"|site_id=="SMYL"|site_id=="SNOW")

wap

### antarctic fur seal

# seal islands # boveng et al. 1998, Hiruki-Raring 2012, 283 +-8

# south orkney Waluda 2010 Increasing up to 2010.

# krause 2022 SSI  

afspop<-read.csv("C:/D1MPA trimestral/Species Pops/antarctic_fur_seal.csv")

ggplot(afspop,aes(Season,CS))+
  geom_smooth(se=F,method="gam")+
  geom_point()+
theme_bw()  +
  ylab("Pup count")+ggtitle(label="a. Cape Shirreff")+


ggplot(afspop,aes(Season,STI))+
  geom_smooth(se=F,method="gam")+
  geom_point()+
  theme_bw()+
  ylab("Pup count")  +ggtitle(label="b. San Telmo Islands")+


ggplot(afspop,aes(Season,SOIpups))+
  geom_smooth(se=F,method="gam")+
  geom_point()+
  theme_bw()  +
  ylab("Pup count")+ggtitle(label="c. South Orkney")+

ggplot(afspop,aes(Season,SOIadults))+
  geom_smooth(se=F,method="gam")+
  geom_point()+
  theme_bw()  +
  ylab("Adult count")+ggtitle(label="d. South Orkney")

