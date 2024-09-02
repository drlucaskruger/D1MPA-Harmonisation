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
library(sf)
library(energy)
gc()

### ---------- grid points ---------------

shs<-data.frame(shapefile("C:/CCAMLR 2024/SC/LKvertices/Grid_D1MPA_Vertices_2024_HS.shp"))

### first, which layers have over 75% of their range within shelf?

head(shs)

shs$SubArea<-shs$GAR_Long_L

summary(as.factor(shs$SubArea))

summary(as.factor(shs$Name_1))
summary(as.factor(shs$Duration))

shs$Scenario<-c("HS")
#shs<-subset(shs,Strata!="SWAP2")

#shs<-subset(shs,Strata!="BSPBS")

head(shs)


df.shs<-data.frame(SA=shs$GAR_Long_L,
                   Name=shs$Name_1,
                 Period=shs$Duration,
                 shs[18:47],
                 shelf=as.factor(shs$Shelf))    

  

df<-df.shs

summary(as.factor(df$ADP_MIG))
summary(as.factor(df$shelf))
summary(as.factor(df$Name))
summary(as.factor(df$Scenario))


df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

df$lon<-shs$x
df$lat<-shs$y

summary(as.factor(df$AFS_BS_SSI))

head(df)
cols_of_interest <- 4:22

# Calculate the 95% quantile for each of these columns
quantiles <- sapply(df[, cols_of_interest], quantile, probs = 0.95, na.rm = TRUE)

# Function to classify values as binary
classify_binary <- function(x, q) {
  ifelse(x > q, 1, 0)
}

# Apply the classification to each column
for (i in seq_along(cols_of_interest)) {
  col <- cols_of_interest[i]
  q <- quantiles[i]
  df[, col] <- classify_binary(df[, col], q)
}




# View the updated data frame
head(df)

summary(as.factor(df$ADP_MIG))
summary(as.factor(df$shelf))


### these are the objects with less than 50% of range on shelf 

head(df)

#df$AFS_AMJ[df$shelf=="0"]<-0
#df$AFS_JFM[df$shelf=="0"]<-0
#df$AFS_JAS[df$shelf=="0"]<-0
#df$SES_OND[df$shelf=="0"]<-0

#df$ADP_MIG[df$shelf=="0"]<-0
#df$CHP_AMJ[df$shelf=="0"]<-0
#df$CHP_JAS[df$shelf=="0"]<-0

#df$AMW_AMJ[df$shelf=="0"]<-0
#df$KrillDens[df$shelf=="0"]<-0
#df$HBW_AMJ[df$shelf=="0"]<-0

#head(df)

dfm<-melt(df[1:34],id.vars=c("SA","Name","Period","shelf"))

head(dfm)



summary(as.factor(dfm$variable))

names(dfm)[names(dfm) == "variable"] <- "Objects"

summary(as.factor(dfm$value))

names(dfm)[names(dfm) == "value"] <- "Occurrence"

dfm$Category[is.na(dfm$Name)]<-"out"

summary(as.factor(dfm$Name))

mpas<-subset(dfm,Name!="out")


summary(as.factor(df$SA))


# domain 1 level
head(df)
dfs<-df # the range in domain 1 is the same through scenarios

D1 <- data.frame(dfs[4:33]) %>%
  #group_by(Scenario) %>%
  summarise_all(sum, na.rm = TRUE)

d1m<-melt(D1)
d1m

names(d1m)[names(d1m) == "value"] <- "D1.range"

d1m$percD1<-d1m$D1.range/23745


# strata level

summary(as.factor(df$SA))
summary(as.factor(df$Name))


head(df)



df2<-df

mpac <- na.omit(data.frame(df2[1:33])) %>%
  group_by(SA,Period,Name) %>%
  summarise_all(sum, na.rm = TRUE)

mpam<-melt(mpac,id.vars=c("SA","Period","Name"))

names(mpam)[names(mpam) == "value"] <- "Z.range"


head(mpac)


mpa.c<-merge(mpam,d1m,by=c("variable"))

head(mpa.c)

md1<-plyr::ddply(mpa.c, c("variable","Name"), summarise,
                      cover=sum(Z.range/D1.range)*100)

head(md1)
d1cast<-dcast(data=md1,formula=variable~Name,value.var = "cover",mean)

write.csv(d1cast,"C:/CCAMLR 2024/SC/table_summary_SpatialUnits_2.csv")


##-------- sum coverage------------------

mpa.c.d1<-plyr::ddply(mpa.c, c("variable","Period"), summarise,
                      cover=sum(Z.range/D1.range))

mpa.481_883<-plyr::ddply(subset(mpa.c,SA=="48.1"|SA=="88.3"), 
                         c("variable","Period"), summarise,
                         cover=sum(Z.range/D1.range))

mpa.481<-plyr::ddply(subset(mpa.c,SA=="48.1"), 
                     c("variable","Period"), summarise,
                     cover=sum(Z.range/D1.range))


mpa.c.d1$area<-("D1")



mpa.481_883$area<-("48.1 & 88.3")
mpa.481$area<-("48.1")

cov<-rbind(mpa.c.d1,mpa.481_883,mpa.481)

head(cov)

covcast<-dcast(data=cov,formula=variable+area~Period,value.var = "cover",mean)



covcast$`Apr to May`[is.na(covcast$`Apr to May`)]<-0

covcast$`Oct to Mar`[is.na(covcast$`Oct to Mar`)]<-0

covcast$`Year-round`[is.na(covcast$`Year-round`)]<-0

covcast$`Oct to Feb`[is.na(covcast$`Oct to Feb`)]<-0

covcast$`Dec to Feb`[is.na(covcast$`Dec to Feb`)]<-0

head(covcast)

# year-round + april and may
am0<-subset(cov,Period=="Year-round"|Period=="Apr to May")

am1<-subset(am0,variable=="CHP_AMJ"|variable=="GEP_AMJ"|variable=="CES_AMJ"|
              variable=="AFS_AMJ"|variable=="SES_AMJ"|variable=="HBW_AMJ"|
              variable=="AMW_AMJ"|variable=="EMP_COLS")

am<-plyr::ddply(am1, c("variable","area"), summarise,
                cover=sum(cover))

summary(am$cover)

am1$variable<-factor(am1$variable,levels=c("GEP_AMJ","CHP_AMJ",
                                           "EMP_COLS","AFS_AMJ","SES_AMJ","CES_AMJ","HBW_AMJ","AMW_AMJ"))

#year-round + Winter

jas0<-subset(cov,Period=="Year-round")
jas1<-subset(jas0,variable=="GEP_JAS"|variable=="CES_JAS"|
               variable=="SES_JAS"|variable=="EMP_COLS")
jas<-plyr::ddply(jas1, c("variable","area"), summarise,
                cover=sum(cover))


jas$variable<-factor(jas$variable,levels=c("GEP_JAS","EMP_COLS",
                                           "SES_JAS","CES_JAS"))

# year round + srping and summer



om0<-subset(cov,Period!="Apr to May")
summary(as.factor(om0$variable))
om1<-subset(om0,variable=="IBA_ADP"|variable=="IBA_CHP"|variable=="IBA_GEP"|variable=="AFS_BS_SSI"|
              variable=="WAGG"|variable=="AFS_JFM"|variable=="SES_JFM"|variable=="SES_OND"|variable=="CES_OND"|
              variable=="ADP_MIG"|variable=="EMP_COLS"|variable=="HBW_JFM"|variable=="AMW_JFM"|
              variable=="KrillDens"|variable=="Krill_JR"|variable=="Krill_SP" |variable=="FISHLA"|variable=="FISHAD")

om<-plyr::ddply(om1, c("variable","area"), summarise,
                 cover=sum(cover))

prot<-rbind(om,jas,am)


summary(as.factor(prot$variable))

prot$variable<-factor(prot$variable,levels=c("FISHLA","FISHAD",
                                             "Krill_SP","KrillDens","Krill_JR",
                                             "CHP_AMJ","IBA_CHP","EMP_COLS",
                                             "ADP_MIG","IBA_ADP",
                                             "GEP_JAS","GEP_AMJ","IBA_GEP",
                                             "SES_JAS","SES_OND","SES_AMJ","SES_JFM",
                                             "CES_JAS","CES_OND","CES_AMJ",
                                             "AFS_JAS","AFS_AMJ","AFS_JFM","AFS_BS_SSI",
                                             "WAGG","HBW_AMJ","HBW_JFM","AMW_AMJ","AMW_JFM"))


whales<-subset(prot,variable=="WAGG"|variable=="HBW_AMJ"|variable=="HBW_JFM"|
                 variable=="AMW_AMJ"|variable=="AMW_JFM")

seals<-subset(prot,variable=="SES_JAS"|variable=="SES_OND"|variable=="SES_AMJ"|
                variable=="SES_JFM"|variable=="CES_JAS"|variable=="CES_OND"|
                variable=="CES_AMJ"|
                variable=="AFS_JAS"|variable=="AFS_AMJ"|
                variable=="AFS_JFM"|variable=="AFS_BS_SSI")


penguins<-subset(prot,variable=="CHP_AMJ"|variable=="IBA_CHP"|variable=="EMP_COLS"|
                   variable=="ADP_MIG"|variable=="IBA_ADP"|
                   variable=="GEP_JAS"|variable=="GEP_AMJ"|variable=="IBA_GEP")

fk<-subset(prot,variable=="Krill_SP"|variable=="KrillDens"|variable=="Krill_JR"|
             variable=="FISHLA"|variable=="FISHAD")




ggplot(fk,aes(variable,cover,colour=area,shape=area,linetype=area))+
  geom_hline(yintercept=0.2,linetype="dotdash")+
  geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+
  ggtitle(label="a. Krill and fish")+



ggplot(penguins,aes(variable,cover,colour=area,shape=area,linetype=area))+
  geom_hline(yintercept=0.5,linetype="dotdash")+
  geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+
  ggtitle(label="b. Penguins")+

ggplot(seals,aes(variable,cover,colour=area,shape=area,linetype=area))+
  geom_hline(yintercept=0.5,linetype="dotdash")+
  geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+
  ggtitle(label="c. Seals")+


ggplot(whales,aes(variable,cover,colour=area,shape=area,linetype=area))+
  geom_hline(yintercept=0.5,linetype="dotdash")+
  geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+
  ggtitle(label="d. Whales")


### -------- additional penguins evaluations----------------

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


library(lmerTest)
library(sjPlot)
library(fitdistrplus)
library(AER)
library(easystats)
library(energy)

cols<-read.csv("C:/CCAMLR 2024/SC/Pygoscelis/CountQuery_V_4_1.csv")

EPSG4326<-CRS("+proj=longlat +datum=WGS84 +no_defs")
head(cols)

summary(as.factor(cols$cammlr_region))


nests<-subset(cols,count_type=="nests")[1:14]

nests<-subset(nests,season_starting>1979)

head(nests)

summary(as.factor(nests$accuracy))


nm<-plyr::ddply(nests, c("site_id","common_name"), summarise,
                Ncounts=length(penguin_count),
                minY=min(season_starting),
                maxY=max(season_starting),
                rangeY=(maxY-minY),
                lon=as.numeric(mean(longitude_epsg_4326)),
                lat=mean(latitude_epsg_4326),
                mean_count=mean(penguin_count),
                sep=(sd(penguin_count)/mean_count)) 


head(nm)
summary(nm$lon)

#write.csv(nm,"C:/CCAMLR 2024/penguin_colony_size.csv")



ggplot((nm),aes(Ncounts,mean_count))+
  #geom_smooth(method="gam",formula=y~s(x,k=5),se=F)+
  geom_point()+
  geom_hline(yintercept=5000,linetype="dashed")+
  geom_hline(yintercept=10000,linetype="dotted")+
  scale_y_log10()+theme_bw()



#IBAS

iadp<-raster("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Non-tracking_layers/IBA_ADP.tif")
ichp<-raster("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Non-tracking_layers/IBA_CHP.tif")
igep<-raster("C:/D1MPA_2024_Harmony/D1MPA_Harmonized/Non-tracking_layers/IBA_GEP.tif")

nesp<-SpatialPointsDataFrame(nm[7:8],nm,proj4string = EPSG4326)

nm_6932<-sp::spTransform(nesp,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))


adp<-subset(nm_6932,common_name=="adelie penguin")
chp<-subset(nm_6932,common_name=="chinstrap penguin")
gep<-subset(nm_6932,common_name=="gentoo penguin")

adp$iba<-(raster::extract(iadp,adp))
chp$iba<-(raster::extract(ichp,chp))
gep$iba<-(raster::extract(igep,gep))



ibas<-rbind(data.frame(adp),data.frame(chp),data.frame(gep))

ibas$iba[is.na(ibas$iba)]<-0

summary(as.factor(ibas$iba))
head(ibas)
summary(ibas$mean_count)

head(ibas)


ggplot(ibas,aes(lon,lat,colour=as.factor(iba)))+geom_point()+
  facet_wrap(common_name~.)


ibaT<-plyr::ddply(ibas, c("iba","common_name"), summarise,
                  Ncols=length(na.omit(mean_count)),
                  Tpop=sum(na.omit(mean_count)),
                  Mpop=mean(na.omit(mean_count)),
                  SDpop=sd(na.omit(mean_count)),
                  SEpop=sqrt(sd(na.omit(mean_count))/(length(na.omit(mean_count))-1)),
                  CI=quantile(na.omit(mean_count),probs=0.25),
                  Tsep=mean(na.omit(sep))) 


iba<-plyr::ddply(subset(ibas,iba=="1"), c("common_name"), summarise,
                 coliba=length(na.omit(mean_count)),
                 popiba=sum(na.omit(mean_count)),
                 ciba=quantile(na.omit(mean_count),probs=0.75))

pop<-plyr::ddply(ibas, c("common_name"), summarise,
                 Ncols=length(na.omit(mean_count)),
                 Tpop=sum(na.omit(mean_count)),
                 CI=quantile(na.omit(mean_count),probs=0.75))


popiba<-merge(pop,iba)


popiba$colsiniba<-popiba$coliba/popiba$Ncols

popiba$popiniba<-popiba$popiba/popiba$Tpop

popiba$sp[popiba$common_name=="adelie penguin"]<-"ADP"
popiba$sp[popiba$common_name=="chinstrap penguin"]<-"CHP"
popiba$sp[popiba$common_name=="gentoo penguin"]<-"GEP"

popiba$ciperc<-popiba$ciba/popiba$popiba

popiba

ggplot(popiba,aes(colsiniba,popiniba,label=sp))+
  geom_errorbar(aes(ymin=popiniba-ciperc,ymax=popiniba+ciperc),width=0.01)+
  geom_label()+theme_bw()+
  xlab("Proportion of D1 colonies in IBA")+
  ylab("Proportion of D1 population in IBA ")

hs<-vect(shapefile("C:/CCAMLR 2024/SC/LKvertices/D1MPA_Vertices_2024_HS.shp"))

plot(hs)


## IBAs representation of colonies

head(ibas)

iba6932<-vect(SpatialPointsDataFrame(coordinates(ibas[12:13]),ibas,
                                proj4string=CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
)

hs<-terra::project(hs,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

plot(iba6932)

same.crs(iba6932,hs)

head(hs)

iba.hs<-merge(data.frame(terra::intersect(iba6932,hs)),data.frame(iba6932),all=T)


head(iba.hs)

ibahs<-data.frame(iba.hs[1:11],Period=iba.hs$Duration)

scens<-(ibahs)

head(scens)

summary(as.factor(scens$Period))
scens$Period[is.na(scens$Period)]<-"Unprotected"

prot<-plyr::ddply(subset(scens,Period!="Unprotected"), c("common_name"), summarise,
                  colMPA=length(na.omit(mean_count)),
                  popMPA=sum(na.omit(mean_count)),
                  ciMPA=quantile(na.omit(mean_count),probs=0.99))



pim<-merge(pop,prot,all=T)

pim$ciperc<-pim$ciMPA/pim$popMPA

pim$colinMPA<-pim$colMPA/pim$Ncols
pim$popinMPA<-pim$popMPA/pim$Tpop

pim$sp[pim$common_name=="adelie penguin"]<-"ADP"
pim$sp[pim$common_name=="chinstrap penguin"]<-"CHP"
pim$sp[pim$common_name=="gentoo penguin"]<-"GEP"

pim

popiba


### ----- buffers-------------


adpbuf<-terra::buffer(adp,width=30000)
chpbuf<-terra::buffer(chp,width=30000)
gepbuf<-terra::buffer(gep,width=30000)


adp.area<-terra::area(adpbuf)
chp.area<-terra::area(chpbuf)
gep.area<-terra::area(gepbuf)

summary(as.factor(hs$Name))
head(hs)

hsS<-subset(hs, hs$Name!="SPZ-LIS")

plot(hsS)

adp.hs<-terra::intersect(vect(adpbuf),hsS)

adp.hs.area<-terra::expanse((adp.hs))

chp.hs<-terra::intersect(vect(chpbuf),hsS)
chp.hs.area<-terra::expanse(chp.hs)

gep.hs<-terra::intersect(vect(gepbuf),hsS)
gep.hs.area<-terra::expanse(gep.hs)




buffcov<-data.frame(sp=c("ADP",
                         "CHP","GEP"),
                    
                    coverage=c(sum(adp.hs.area)/adp.area,
                               
                               
                               sum(chp.hs.area)/chp.area,
                              
                               
                               sum(gep.hs.area)/gep.area))


### -----------exclusion of SOI-------------

summary(as.factor(hs$strata))

head(hs)
hs2<-subset(hs,hsS$Name!="GPZ-SOI")

plot(hs)
plot(hs2)

iba.hs2<-merge(data.frame(terra::intersect(iba6932,hs2)),data.frame(iba6932),all=T)
head(iba.hs2)

ibahs2<-data.frame(iba.hs2[1:11],Period=iba.hs2$Duration)

scens2<-(ibahs2)

head(scens2)

summary(as.factor(scens2$Period))
scens2$Period[is.na(scens2$Period)]<-"Unprotected"

prot2<-plyr::ddply(subset(scens2,Period!="Unprotected"), c("common_name"), summarise,
                   colMPA=length(na.omit(mean_count)),
                   popMPA=sum(na.omit(mean_count)),
                   ciMPA=quantile(na.omit(mean_count),probs=0.99))



pim2<-merge(pop,prot2,all=T)

pim2$ciperc<-pim2$ciMPA/pim2$popMPA

pim2$colinMPA<-pim2$colMPA/pim2$Ncols
pim2$popinMPA<-pim2$popMPA/pim2$Tpop

pim2$sp[pim2$common_name=="adelie penguin"]<-"ADP"
pim2$sp[pim2$common_name=="chinstrap penguin"]<-"CHP"
pim2$sp[pim2$common_name=="gentoo penguin"]<-"GEP"

pim2

summary(as.factor(hs2$Duration))

hsS2<-subset(hs2,hs2$Duration!="Apr to May")

adp.hs2<-terra::intersect(vect(adpbuf),hsS2)
adp.hs.area2<-terra::expanse(adp.hs2)

chp.hs2<-terra::intersect(vect(chpbuf),hsS2)
chp.hs.area2<-terra::expanse(chp.hs2)

gep.hs2<-terra::intersect(vect(gepbuf),hsS2)
gep.hs.area2<-terra::expanse(gep.hs2)



buffcov2<-data.frame(sp=c("ADP",
                          "CHP","GEP"),
                     
                     coverage=c(sum(adp.hs.area2)/adp.area,
                                
                                sum(chp.hs.area2)/chp.area,
                                
                                sum(gep.hs.area2)/gep.area))



### --------both data together--------------

head(buffcov)
head(buffcov2)

buffcov$area<-c("D1")
buffcov2$area<-c("48.1 & 88.3")

bc<-rbind(buffcov,buffcov2)

head(buffcov)
buffcov2


### --------- also the population importance data----------

head(pim) # all D1
head(pim2) # without SOI

pim$area<-c("D1")
pim2$area<-c("48.1 & 88.3")


pimpa<-rbind(pim,pim2)

pimpaB<-merge(pimpa,bc,all=T)
head(pimpaB)

### ------- plots of coverages-----------

(ggplot((pimpa),aes(colinMPA,popinMPA,label=sp))+
   geom_hline(yintercept=0.5,linetype="dashed")+
   geom_errorbar(aes(ymin=popinMPA-ciperc,ymax=popinMPA+ciperc))+
   geom_label()+facet_wrap(area~.)+
   theme_bw()+
   xlab("Proportion of D1 colonies within MPA")+
   ylab("Proportion of D1 population within MPA")+
   ggtitle(label="a.")+
   scale_colour_manual(values=c("red3","blue4")))/
  
  
  
  (
    ggplot(pimpaB,aes(coverage,popinMPA,label=sp))+
      geom_hline(yintercept=0.5,linetype="dashed")+
      geom_errorbar(aes(ymin=popinMPA-ciperc,ymax=popinMPA+ciperc))+
      geom_label()+facet_wrap(area~.)+
      theme_bw()+
      xlab("Proportion of D1 colonies buffer within MPA")+
      ylab("Proportion of D1 population within MPA")+
      ggtitle(label="b.")+
      scale_colour_manual(values=c("red3","blue4")))


##-------------Colonies trends------------------
library(lmerTest)

### PS let's use as many data as possible

head(nests)


nests2<-merge(nests,nm,by=c("site_id","common_name"))

summary(nests2$Ncounts) # it is not ideal, but, as we wish to identify potential local changes in time
# lets use data even for colonies with only two counts, as it is best to do it than 
# to limit the number of colonies


nests2<-subset(nests2,Ncounts>1)

summary(as.factor(nests2$accuracy))

nests.ac<-na.omit(subset(nests2,accuracy<5)) ### accuracy 4 and 5 are the less accurate ones



adp<-subset(nests.ac,common_name=="adelie penguin")
chp<-subset(nests.ac,common_name=="chinstrap penguin")
gep<-subset(nests.ac,common_name=="gentoo penguin")


### ----------- Adelie Penguin -------------


poisson.mtest(adp$penguin_count,R=299)  ##test for poisson distribution

glmm1<-glmer(penguin_count~scale(season_starting)+
               (scale(season_starting)|site_id),data=adp,family="poisson")
glm1<-glm(penguin_count~scale(season_starting),data=adp,family="poisson")

anova(glmm1,glm1) # mixed model is way better
10271/8920849

summary(glmm1)  # global decrease, but not significant


glmm1

(2.953+1.063)/((2.953+1.063)+(7.3754+0.2214))  # colony explains 34.6% of variability

# (random intercept + random slope)/ ((random intercept + random slope)+(fixed intercept+fixed slope))


adp$pred<-predict(glmm1,newdata=adp,type="re")

plot((adp$penguin_count),(adp$pred))

cor((adp$penguin_count),(adp$pred)) # very good prediction 


adp.re<-data.frame(ranef(glmm1),species=c("ADP"))

summary(glmm1)
adp.re$trend<-adp.re$condval-0.221


adpre<-subset(adp.re,term=="scale(season_starting)")

aredf<-data.frame(site_id=adpre$grp,slope=adpre$trend,sd=adpre$condsd,species=adpre$species)


acoords<-plyr::ddply(adp, c("site_id"), summarise,
                     lon=mean(longitude_epsg_4326),
                     lat=mean(latitude_epsg_4326),
                     Ncounts=length(penguin_count),
                     mean_count=mean(penguin_count),
                     sep=(sd(penguin_count)/mean_count))



aranef<-merge(aredf,acoords)

write.csv(aranef,"C:/CCAMLR 2024/SC/adelie_penguin_colony_trend.csv")

###------- Chinstrap Penguin--------------------

poisson.mtest(chp$penguin_count,R=299)  ##test for poisson distribution

glmm2<-glmer(penguin_count~scale(season_starting)+
               (scale(season_starting)|site_id),data=chp,family="poisson")
glm2<-glm(penguin_count~scale(season_starting),data=chp,family="poisson")

anova(glmm2,glm2) # mixed model is way better
14720/2133114
summary(glmm2)  # significant decrease


glmm2

(3.087+1.235)/((3.087+1.235)+(5.2510+0.6009))  # colony explains 42.5% of variability :o

# (random intercept + random slope)/ ((random intercept + random slope)+(fixed intercept+fixed slope))


chp$pred<-predict(glmm2,newdata=chp,type="re")

plot((chp$penguin_count),(chp$pred))

cor((chp$penguin_count),(chp$pred)) # very good prediction 


chp.re<-data.frame(ranef(glmm2),species=c("CHP"))

summary(glmm2)
chp.re$trend<-chp.re$condval-0.6


chpre<-subset(chp.re,term=="scale(season_starting)")

credf<-data.frame(site_id=chpre$grp,slope=chpre$trend,sd=chpre$condsd,species=chpre$species)


ccoords<-plyr::ddply(chp, c("site_id"), summarise,
                     lon=mean(longitude_epsg_4326),
                     lat=mean(latitude_epsg_4326),
                     Ncounts=length(penguin_count),
                     mean_count=mean(penguin_count),
                     sep=(sd(penguin_count)/mean_count))

cranef<-merge(credf,ccoords)

write.csv(cranef,"C:/CCAMLR 2024/SC/chinstrap_penguin_colony_trend.csv")


###--------gentooo penguins---------



poisson.mtest(gep$penguin_count,R=299)  ##test for poisson distribution

glmm3<-glmer(penguin_count~scale(season_starting)+
               (scale(season_starting)|site_id),data=gep,family="poisson")
glm3<-glm(penguin_count~scale(season_starting),data=gep,family="poisson")

anova(glmm3,glm3) # mixed model is way better

71566/937940

summary(glmm3)  # significant global increase


glmm3

(1.8523+0.6734)/((1.8523+0.6734)+(6.0173+0.4158))  # colony explains 28.2% of variability :o

# (random intercept + random slope)/ ((random intercept + random slope)+(fixed intercept+fixed slope))


gep$pred<-predict(glmm3,newdata=gep,type="re")

plot((gep$penguin_count),(gep$pred))

cor((gep$penguin_count),(gep$pred)) # good prediction 


  gep.re<-data.frame(ranef(glmm3),species=c("GEP"))

summary(glmm3)
gep.re$trend<-gep.re$condval+0.4158


gepre<-subset(gep.re,term=="scale(season_starting)")

gredf<-data.frame(site_id=gepre$grp,slope=gepre$trend,sd=gepre$condsd,species=gepre$species)


gcoords<-plyr::ddply(gep, c("site_id"), summarise,
                     lon=mean(longitude_epsg_4326),
                     lat=mean(latitude_epsg_4326),
                     Ncounts=length(penguin_count),
                     mean_count=mean(penguin_count),
                     sep=(sd(penguin_count)/mean_count))

granef<-merge(gredf,gcoords)

write.csv(granef,"C:/CCAMLR 2024/SC/gentoo_penguin_colony_trend.csv")



## ------plots------------

head(aranef)
head(cranef)
head(granef)
ranef<-rbind(aranef,cranef,granef)

head(aranef)

length(aranef$slope[aranef$slope<0])/length(aranef$slope)
length(cranef$slope[cranef$slope<0])/length(cranef$slope)
length(granef$slope[granef$slope<0])/length(granef$slope)


ggplot()+scale_x_log10()+scale_y_log10()+
  geom_point(data=adp,aes(x=penguin_count+1,y=pred+1),colour="black",shape="circle",alpha=0.2)+
  geom_point(data=chp,aes(x=penguin_count+1,y=pred+1),colour="green3",shape="square",alpha=0.2)+
geom_point(data=gep,aes(x=penguin_count+1,y=pred+1),colour="red3",shape="triangle",alpha=0.2)+
  xlab("Observed number of nests")+ylab("Model-estimated number of nests")+
  theme_bw()+ggtitle(label="a. GLMM accuracy")+


ggplot(ranef,aes(Ncounts,mean_count+1,linetype=species))+
  geom_smooth(aes(colour=species),method="lm",se=F,formula=y~log(x))+
  geom_point(aes(colour=species,shape=species),alpha=0.2)+
  scale_y_log10()+
  theme_bw()+xlab("Number of season with counts")+
  ylab("Mean number of nests")+geom_hline(yintercept = 10000,linetype="dashed")+
  geom_text(aes(x=50,y=14000),label="10,000")+
  scale_colour_manual(values=c("black","green3","red3"))+
  scale_shape_manual(values=c("circle","square","triangle"))+
  ggtitle(label="b. Data availability")+

ggplot(ranef,aes(mean_count+1,slope,linetype=species))+
  geom_point(aes(colour=species,shape=species),alpha=0.2)+
  geom_smooth(aes(colour=species),method="gam",se=F,formula=y~s(x,k=4))+
  scale_x_log10()+
  theme_bw()+xlab("Mean number of counts")+
  ylab("Colony trend")+#geom_hline(yintercept = 10000,linetype="dashed")+
  #geom_text(aes(x=55,y=14000),label="10,000")+
  scale_colour_manual(values=c("black","green3","red3"))+
  scale_shape_manual(values=c("circle","square","triangle"))+
  ggtitle(label="c. Trend by colony size")+

ggplot(ranef,aes(lat,slope,linetype=species))+
  geom_smooth(method="lm",se=F,aes(colour=species))+
  geom_point(aes(colour=species,shape=species),alpha=0.2)+
  #scale_y_log10()+
  theme_bw()+xlab("Colony latitude")+
  ylab("Colony trend")+#geom_hline(yintercept = 10000,linetype="dashed")+
  #geom_text(aes(x=55,y=14000),label="10,000")+
  scale_colour_manual(values=c("black","green3","red3"))+
  scale_shape_manual(values=c("circle","square","triangle"))+
  ggtitle(label="d. Geographical trend")


###------ Colony trends withing MUs---------


head(ranef)

rvect<-vect(SpatialPointsDataFrame(ranef[5:6],ranef,proj4string = EPSG4326))
plot(rvect)

rvect<-terra::project(rvect,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

plot(rvect)

mus<-vect(shapefile("C:/CCAMLR 2024/Shapefiles/CMU_03/03/MUs.shp"))

rint<-terra::intersect((rvect),mus)


rint$Unit[rint$Unit=="EI" | rint$Unit=="SSIW"]<-"SSIW + EI"


head(rint)

rint$Unit<-factor(rint$Unit,
                  levels=c("MB+SWAP","GC","GS","BS","JOIN+PB",
                           "SSIW + EI","SOI"))

rdf<-data.frame(rint)

head(rdf)


rdfmed<-plyr::ddply(rdf, c("Unit","species"), summarise,
                    slopemed=median(slope),
                    slopeup=quantile(slope,prob=0.75),
                    slopedown=quantile(slope,prob=0.25),
                    sizemed=median(mean_count),
                    sizeup=quantile(mean_count,prob=0.75),
                   sizedown=quantile(mean_count,prob=0.25))


(ggplot(subset(rdfmed,species=="ADP"),
       aes(sizemed,slopemed,label=Unit))+
    geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(ymin=slopedown,ymax=slopeup))+
  geom_errorbarh(aes(xmin=sizedown,xmax=sizeup))+
  geom_label()+
  theme_bw()+xlab("Colony size")+ylab("Colony trend")+
  scale_x_log10()+ggtitle(label="a. Adelie penguin"))/


(ggplot(subset(rdfmed,species=="CHP"),
       aes(sizemed,slopemed,label=Unit))+
   geom_hline(yintercept=0,linetype="dashed")+
   geom_errorbar(aes(ymin=slopedown,ymax=slopeup))+
  geom_errorbarh(aes(xmin=sizedown,xmax=sizeup))+
  geom_label()+
  theme_bw()+xlab("Colony size")+ylab("Colony trend")+
  scale_x_log10(limits=c(10,10000))+ggtitle(label="b. Chinstrap penguin")
)/

(ggplot(subset(rdfmed,species=="GEP"),
       aes(sizemed,slopemed,label=Unit))+
   geom_hline(yintercept=0,linetype="dashed")+
   geom_errorbar(aes(ymin=slopedown,ymax=slopeup))+
  geom_errorbarh(aes(xmin=sizedown,xmax=sizeup))+
  geom_label()+
  theme_bw()+xlab("Colony size")+ylab("Colony trend")+
  scale_x_log10()+ggtitle(label="c. Gentoo penguin"))


# trends

ggplot(subset(rdf,species=="ADP"),aes(Unit,slope))+geom_boxplot()+
  coord_flip()+
  theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Spatial units")+ylab("Colony trend")+
  ggtitle(label="a. Adelie penguin trend")+
  
  ggplot(subset(rdf,species=="CHP"),aes(Unit,slope))+geom_boxplot()+
  coord_flip()+
  theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Spatial units")+ylab("Colony trend")+
  ggtitle(label="b. Chinstrap penguin trend")+
  
  ggplot(subset(rdf,species=="GEP"),aes(Unit,slope))+geom_boxplot()+
  coord_flip()+
  theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Spatial units")+ylab("Colony trend")+
  ggtitle(label="c. Gentoo penguin trend")+
  
  
  # colony size
    
ggplot(subset(rdf,species=="ADP"),aes(Unit,mean_count))+geom_boxplot()+
    coord_flip()+
    theme_bw()+scale_y_log10()+
    xlab("Spatial units")+ylab("Nests")+
    ggtitle(label="d. Adelie penguin colony size")+
  
  ggplot(subset(rdf,species=="CHP"),aes(Unit,mean_count))+geom_boxplot()+
  coord_flip()+
  theme_bw()+scale_y_log10()+
  xlab("Spatial units")+ylab("Nests")+
  ggtitle(label="e. Chinstrap penguin colony size")+
  
  
  ggplot(subset(rdf,species=="GEP"),aes(Unit,mean_count))+geom_boxplot()+
  coord_flip()+
  theme_bw()+scale_y_log10()+
  xlab("Spatial units")+ylab("Nests")+
  ggtitle(label="f. Gentoo penguin colony size")
  

  
  
  # data availability
  

    ggplot(subset(rdf,species=="ADP"),aes(Unit,Ncounts))+geom_boxplot()+
    coord_flip()+
    theme_bw()+scale_y_log10()+
    xlab("Spatial units")+ylab("Number of counts")+
    ggtitle(label="g. Adelie penguin data")+



  
  ggplot(subset(rdf,species=="CHP"),aes(Unit,Ncounts))+geom_boxplot()+
  coord_flip()+
  theme_bw()+scale_y_log10()+
  xlab("Spatial units")+ylab("Number of counts")+
  ggtitle(label="h. Chinstrap penguin data availability")+


  ggplot(subset(rdf,species=="GEP"),aes(Unit,Ncounts))+geom_boxplot()+
  coord_flip()+
  theme_bw()+scale_y_log10()+
  xlab("Spatial units")+ylab("Number of counts")+
  ggtitle(label="i. Gentoo penguin data availability")





###------------ colony buffers ----------
library(track2KBA)

### --------- Harmony Point------

df1<-read.csv("C:/Chinstrap_Priorities/GPS/HP_GPS_DATA.csv")

hp1<-data.frame(Id=df1$filename,Timestamp=df1$date,location.lat=df1$Lat,location.lon=df1$Long)


hp1$Timestamp<-as.POSIXct(hp1$Timestamp, 
                          format = "%Y-%m-%d %H:%M:%S", tz = "GMT")


tr<-read.csv("C:/Chinstrap_Priorities/GPS/tracking_raw.csv")
tail(tr)


tr$Timestamp<-as.POSIXct(tr$Timestamp, 
                         format = "%d/%m/%Y %H:%M:%S", tz = "GMT")

hp<-rbind(hp1,tr[2:5])

plot(hp$location.lon,hp$location.lat)

hp$date_gmt=paste(year(hp$Timestamp),month(hp$Timestamp),day(hp$Timestamp),sep="-")
hp$time_gmt=paste(hour(hp$Timestamp),minute(hp$Timestamp),second(hp$Timestamp),sep=":")


head(hp)

hp<- hp[!duplicated(hp[, c("Id","Timestamp")]), ]

dataGroup <- formatFields(
  dataGroup = hp, 
  fieldID   = "Id", 
  fieldDate = "date_gmt", 
  fieldTime = "time_gmt",
  fieldLon  = "location.lon", 
  fieldLat  = "location.lat"
)

str(dataGroup)

summary(as.factor(dataGroup$ID))

colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 1,      # kilometers
  returnBuff = 1,
  duration   = 1,      # hours
  rmNonTrip  = T
)

#calculate trips caracteristics

sumTripsHP <- tripSummary(trips = trips, colony = colony)


sthp<-data.frame(sumTripsHP)



###--------Kopaitic-------------


#kop<-read.delim("C:/Chinstrap_Priorities/KOPAITIC_GPSTDR_DATA.csv",sep="\t")


kop.gps<-na.omit(data.frame(Id=df1$filename,
                            Timestamp=as.POSIXct(strptime(paste(df1$Date, df1$Time), 
                                                          format = "%d/%m/%Y %H:%M:%S", tz = "GMT")),
                            location.lat=df1[10],location.lon=df1[11]))
rm(df1,combined_data)

gc()

kop.gps2<-subset(kop.gps,location.lat<(-62.75))
kop.gps2<-subset(kop.gps2,location.lat>(-64))
kop.gps2<-subset(kop.gps2,location.lon<(-57.5))

ggplot(kop.gps2,aes(location.lon,location.lat))+geom_point()

length(unique(kop.gps2$Id))


kop<-kop.gps2

write.csv(kop,"C:/KOPAITIC_ECA59/kopaitic_tracking.csv")

kop$date_gmt=paste(year(kop$Timestamp),month(kop$Timestamp),day(kop$Timestamp),sep="-")
kop$time_gmt=paste(hour(kop$Timestamp),minute(kop$Timestamp),second(kop$Timestamp),sep=":")


head(kop)

kop<- kop[!duplicated(kop[, c("Id","Timestamp")]), ]

dataGroup <- formatFields(
  dataGroup = kop, 
  fieldID   = "Id", 
  fieldDate = "date_gmt", 
  fieldTime = "time_gmt",
  fieldLon  = "location.lon", 
  fieldLat  = "location.lat"
)

str(dataGroup)

summary(as.factor(dataGroup$ID))

colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 1,      # kilometers
  returnBuff = 1,
  duration   = 1,      # hours
  rmNonTrip  = T
)

#calculate trips caracteristics

sumTripsKOP <- tripSummary(trips = trips, colony = colony)



sthp<-data.frame(sumTripsHP)
stkp<-data.frame(sumTripsKOP)


sthp$Colony<-c("HP")
stkp$Colony<-c("KP")


st<-rbind(sthp,stkp)

head(st)

st$month<-month(st$departure)
ggplot(st,aes(Colony,max_dist,fill=as.factor(month)))+geom_boxplot()


st$mday<-mday(st$departure)

st$startM[st$month=="12"]<-0
st$startM[st$month=="1"]<-1
st$startM[st$month=="2"]<-2


st$start<-(st$mday/31)+st$startM

st$year<-ifelse(st$month=="12",2023,2024)

st$hms<-substring(st$departure,first=12,last=19)

st$hpdate<-as.POSIXct(paste(paste(st$year,st$month,st$mday,sep="-"),st$hms), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

#st$start<-decimal_date(st$departure)

#st<-subset(st,start<2.5 & max_dist<100)

st$year<-year(st$departure)

write.csv(kop.gps2,"C:/Chinstrap_Priorities/KOP_GPS.csv")
write.csv(st,"C:/Chinstrap_Priorities/ForagingTrips.csv")

head(st)


  
  
( ggplot(subset(st,Colony=="KP"),aes(departure,max_dist))+geom_point()+
  
  # geom_hline(yintercept=18,linetype="dashed")+
  geom_hline(yintercept=5,linetype="dashed")+
  theme_bw()+
  xlab("Date")+
    ylab("Maximum distance from the colony")+
    ggtitle(label="a. Kopaitic Island"))/

(ggplot(subset(st,Colony=="HP"),aes(hpdate,max_dist))+geom_point()+

  geom_hline(yintercept=18,linetype="dashed")+
  #geom_hline(yintercept=5,linetype="dotted")+
  theme_bw()+
  xlab("Date")+
   ylab("Maximum distance from the colony")+
   ggtitle(label="b. Harmony Point"))



summary(st$max_dist[st$Colony=="KP"])

length(st$tripID[st$max_dist<=5 & st$Colony=="KP"])/
  length(st$tripID[st$Colony=="KP"])
head(sthp)

st$Colony<-c("HP")
length(sthp$tripID[st$max_dist<=15 & st$Colony=="HP"])/
  length(sthp$tripID[st$Colony=="HP"])



### --------- representative objectives-------------------



# first let's create a single shapefile from the scenarios, to make out life easier

head(df)

epsg6932<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"



dfsp<-vect(df,geom=c("lon","lat"),crs=epsg6932)

### load and rasterize represeantation objects



# Load the mask 
mask <- raster("C:/D1MPA trimestral/mask.tif") # mask

plot(mask)

# Set working directory 
setwd("C:/CCAMLR 2024/SC/Representation Objectives/shapefiles")


# output directory
output_dir <- "C:/CCAMLR 2024/SC/Representation Objectives/geotiff"

# List all shapefiles in the directory
shapefiles <- list.files(pattern = "\\.shp$")


# Loop through shapefiles
for (shp_file in shapefiles) {
  # Read the shapefile
  shp <- st_read(shp_file)
  
  # Convert the shapefile to raster using the mask
  rasterized <- rasterize(shp, mask, field = 1)
  covered<-cover(rasterized,mask)
  # Define the output raster file name
  output_file <- paste0(output_dir, gsub(".shp", "", shp_file), "_raster.tif")
  
  # Save the raster file
  writeRaster(covered, filename = output_file, format = "GTiff", overwrite = TRUE)
}



### load generated rasters, create a stack and extract values into the grid


# Set the directory where TIF files are located
tif_directory3 <- "C:/CCAMLR 2024/SC/Representation Objectives/geotiff"

# List all TIF files in the directory
tif_files3 <- list.files(tif_directory3, pattern = ".tif$", full.names = T)


tif_files3

# Load all TIF files into a raster stack

raster_stack3 <- terra::rast(tif_files3)

plot(raster_stack3[[17:23]])

ext.st<-terra::extract(raster_stack3, dfsp)
ext.df<-data.frame(ext.st)
head(ext.df)
head(df)

df4<-data.frame(df[35:36],df[1:33],ext.df[2:35])

head(df4)


summary(as.factor(df4$geotiffobj2_BioPel03_raster))


write.csv(df4,"C:/CCAMLR 2024/SC/ConsObj_binary_grid.csv")

#df4<-read.csv("C:/CCAMLR 2024/SC/ConsObj_binary_grid.csv")
# domain 1 level

head(df4)


D1r <- data.frame(df4[7],df4[36:69]) %>%
  #group_by(Scenario) %>%
  summarise_all(sum, na.rm = TRUE)

d1rm<-melt(D1r)

names(d1rm)[names(d1rm) == "value"] <- "D1.range"

d1rm

# MPA
head(df4)

mpacr <- na.omit(data.frame(df4[3],df4[5],df4[36:69])) %>%
  group_by(SA,Period) %>%
  summarise_all(sum, na.rm = TRUE)

mparm<-melt(mpacr,id.vars=c("Period","SA"))

names(mparm)[names(mparm) == "value"] <- "Z.range"


#mpa.b<-merge(mpam,saom,by=c("variable","SA"))

mpa.rc<-merge(mparm,d1rm,by=c("variable"),all=T)

head(mpa.rc)

summary(as.factor(mpa.rc$Scenario))



mpa.rc.d1<-plyr::ddply(mpa.rc, c("variable"), summarise,
                       cover=sum(Z.range/D1.range))

mpa.r481_883<-plyr::ddply(subset(mpa.rc,SA=="48.1"|SA=="88.3"), 
                          c("variable"), summarise,
                          cover=sum(Z.range/D1.range))

mpa.r481<-plyr::ddply(subset(mpa.rc,SA=="48.1"), 
                      c("variable"), summarise,
                      cover=sum(Z.range/D1.range))


mpa.rc.d1$area<-("D1")
mpa.r481_883$area<-("48.1 & 88.3")
mpa.r481$area<-("48.1")

covr<-rbind(mpa.rc.d1,mpa.r481_883,mpa.r481)


covr$achievement<-ifelse(covr$cover>=0.1,1,0)



head(covr)


covrm<-plyr::ddply(na.omit(covr), 
                   c("area"), summarise,
                   cover=mean(achievement),
                   cover.se=sqrt(sd(achievement)/length(achievement)))

covrm




ggplot(covrm,aes(area,cover))+
  #geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_errorbar(aes(ymin=cover-cover.se,ymax=cover+cover.se),width=0.1, position=position_dodge(width=0.9))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+#facet_wrap(Period~.)+
  ylab("Proportion of targets achieved")+
  ggtitle(label="<. Representation objectives")




covr$obj<-substring(covr$variable,first=8,last=11)
tail(covr)


covr2<-subset(covr,area=="D1")

summary(as.factor(covr2$obj))

ggplot(covrm,aes(area,cover))+
  #geom_linerange(position = position_dodge(.9),aes(ymin=0,ymax=cover))+
  geom_errorbar(aes(ymin=cover-cover.se,ymax=cover+cover.se),width=0.1, position=position_dodge(width=0.9))+
  geom_point(size=3,position=position_dodge(width=0.9))+
  coord_flip()+
  theme_bw()+#facet_wrap(Period~.)+
  ylab("Proportion of targets achieved")+
  ggtitle(label="a. Representation objectives achieved")+

ggplot(subset(covr2,obj=="obj1"),
       aes(reorder(variable,+cover),cover))+
  geom_point(size=3)+coord_flip()+
  geom_hline(yintercept=0.1,linetype="dashed")+
  theme_bw()+xlab("Objects")+ylab("Proportion coverage")+
  ggtitle(label="b. Objective 1")+


ggplot(subset(covr2,obj=="obj2"),
       aes(reorder(variable,+cover),cover))+
  geom_point(size=3)+coord_flip()+
  geom_hline(yintercept=0.1,linetype="dashed")+
  theme_bw()+xlab("Objects")+ylab("Proportion coverage")+
  ggtitle(label="c. Objective 2")+


ggplot(subset(covr2,obj=="obj3"|obj=="obj4"|obj=="obj5"),
       aes(reorder(variable,+cover),cover))+
  geom_point(size=3)+coord_flip()+
  geom_hline(yintercept=0.1,linetype="dashed")+
  theme_bw()+xlab("Objects")+ylab("Proportion coverage")+
  ggtitle(label="d. Objectives 3,4, and 5")

