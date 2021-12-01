# Date : 29/06/2021 
# Author : julien ruffault (julien.ruff@gmail.com)
# Get data from fontblanche, gapfill with SAFRAN-based relationships and put into the right foramt required to un SurEau-Ecos (v1.0)


# <<< required format for SurEau-Ecos >>>>
# DATE	      Tair_min	Tair_max	Tair_mean	RG_sum	PPT_sum	RHair_min	RHair_mean	RHair_max  WS
# DD/MM/YYYY  (°C)      (°C)      (°C)      (MJ/m2)     (mm)     (%)      (%)         (%)    (m/s)


# projection LIIE/ Code numerique est : 27572
# projection L93 / le Code numerique est : 2154
# Projection WGS84 / code numerique 4326
# Fontblanche	coordinates  : 43.24079	5.67865


rm(list=ls())
library(sp)
library(raster)
library(lubridate)

library(MBC)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

source('RHfromSHandT.R')
# path to Fontblanche climate datasets 
FONTBLANCHE_path =   "/Volumes/paca-urfm/URFM/Implantations_Experimentales/Font-Blanche/Climate/Meteo/"
# path to SAFRAN data 
SAFRAN_path = "/Volumes/paca-urfm/URFM/PEF/Transferts-TEMP/Safran/"


# get_SAFRAN point and MEF ------------------------------------------------
# coordonnes Safran en LII
SafranCoord<-read.table(paste0(SAFRAN_path,"/coord-grille-sim2-fev2018.csv"), sep=";", h=T)
SafranCoord[,c(4,5)] <- SafranCoord[,c(4,5)]*100  #1 attention multiplié par 100 car comme ca dans le fichier orignial 
head(SafranCoord)
plot(SafranCoord$lambx,SafranCoord$lamby)
SafranCoord.sp = SafranCoord
coordinates(SafranCoord.sp) <- ~lambx+lamby
crs(SafranCoord.sp) = CRS("+init=EPSG:27572")

# coordonnes fontblanche 
FONTBLANCHE_coord= data.frame(LAT=43.24079,LON=	5.67865) # fichier avec les coordonnes des sites etudies
FONTBLANCHE_coord.sp <-  FONTBLANCHE_coord
coordinates(FONTBLANCHE_coord.sp) <- ~LON+LAT
crs(FONTBLANCHE_coord.sp) = CRS("+init=EPSG:4326")
FONTBLANCHE_coord.sp.LIIE = spTransform(FONTBLANCHE_coord.sp,CRS("+init=EPSG:27572"))


d <- pointDistance(FONTBLANCHE_coord.sp.LIIE,SafranCoord.sp, lonlat=F)
PIX =which.min(d)[1]
PIX

io =read.table(file=paste0(SAFRAN_path,'/quotidiennes_1959_2019_maille_',PIX,'.csv.gz'),sep=';',h=T)

# put into SUREAU Format
RRR       <- data.frame(DATE       = as.Date(as.character(io$date),format='%d/%m/%Y'),
                        Tair_min   = pmin(io[, "tinf_h_q"],io[, "t_q" ]), ## Tmin(degC) /corrige pour les cas ou tmin> tmoy dans les donnees SAFRAN (ca arrive)
                        Tair_max   = pmax(io[, "tsup_h_q"],io[, "t_q" ]),# Tmax(degC) corrige pour les cas ou tmax< tmoy dans les donnees SAFRAN (ca arrive)
                        Tair_mean  = io[, "t_q" ],
                        RG_sum      = io[, "ssi_q"] / 100,
                        PPT_sum     = io[,"preliq_q"]+io[,"prenei_q"],## precipitation (mm)
                        RHair_min  = RHfromSHandT(Tc=pmax(io[, "tsup_h_q"],io[, "t_q" ]),HS = io[, "q_q"] /1000), 
                        RHair_mean = RHfromSHandT(Tc=io[, "t_q" ],HS = io[, "q_q"] /1000), 
                        WS_mean    = io[, "ff_q"]) ## Wind speed (m/s)
RRR$RHair_max = pmin(100,2*RRR$RHair_mean-RRR$RHair_min)

# gget fontblanche data and correction with SAFRAN 

data_FON = NULL

# note : shortwave radiation in W.m-2 in fontblanche




for (Y  in c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))
{
  print(Y)
  io=read.csv(file=paste0(FONTBLANCHE_path,'FBn_',Y,'_Meteo_L2B_hourly.csv'),sep=';',dec='.')
  io[io==-9999] <- NA
  as.POSIXct(io$TIMESTAMP,format=)
  io$TIME   = as.POSIXct(as.character(io$TIMESTAMP),format=c('%d/%m/%Y %H:%M'))
  io$DOY= yday(io$TIME) 
  
  temp_data<- data.frame(Tair_min   = aggregate(io$TA,by=list(io$DOY),min)$x,
                        Tair_max   = aggregate(io$TA,by=list(io$DOY),max)$x,# Tmax(degC) corrige pour les cas ou tmax< tmoy dans les donnees SAFRAN (ca arrive)
                        Tair_mean  = aggregate(io$TA,by=list(io$DOY),mean)$x,
                        RG_sum     = aggregate(io$SW_IN,by=list(io$DOY),FUN=function(x) (sum(x*1800/1e6)))$x,
                        PPT_sum    = aggregate(io$P,by=list(io$DOY),sum)$x,
                        RHair_min  = aggregate(io$RH,by=list(io$DOY),min)$x,
                        RHair_max  = aggregate(io$RH,by=list(io$DOY),max)$x,# 
                        RHair_mean = aggregate(io$RH,by=list(io$DOY),mean)$x,
                        WS_mean    = aggregate(io$WS,by=list(io$DOY),mean)$x) ## Wind speed (m/s)
  temp_data$DATE = seq.Date(from=as.Date(paste0('01/01/',Y),format='%d/%m/%Y'),to = as.Date(paste0('31/12/',Y),format='%d/%m/%Y'),by='day')
  
  data_FON=rbind(data_FON,temp_data)              
}
  
  

data_FON_corrected = data_FON
head(data_FON_corrected)

# correction pour la temperature moyenne 
io = left_join(data_FON[,c('DATE','Tair_mean')],RRR[,c('DATE','Tair_mean')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$Tair_mean),],RRR[,c('DATE','Tair_mean')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$Tair_mean.y)
data_FON_corrected[is.na(data_FON_corrected$Tair_mean),'Tair_mean'] <- azer$mhat.p
plot(AAA$Tair_mean.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)





# correction pour la temperature min
io = left_join(data_FON[,c('DATE','Tair_min')],RRR[,c('DATE','Tair_min')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$Tair_min),],RRR[,c('DATE','Tair_min')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$Tair_min.y)
data_FON_corrected[is.na(data_FON_corrected$Tair_min),'Tair_min'] <- azer$mhat.p
plot(AAA$Tair_min.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)






# correction pour la temperature max
io = left_join(data_FON[,c('DATE','Tair_max')],RRR[,c('DATE','Tair_max')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$Tair_max),],RRR[,c('DATE','Tair_max')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$Tair_max.y)
data_FON_corrected[is.na(data_FON_corrected$Tair_max),'Tair_max'] <- azer$mhat.p
plot(AAA$Tair_max.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)



# correction pour les precipitations
io = left_join(data_FON[,c('DATE','PPT_sum')],RRR[,c('DATE','PPT_sum')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$PPT_sum),],RRR[,c('DATE','PPT_sum')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$PPT_sum.y,ratio=T,trace=0.5)
data_FON_corrected[is.na(data_FON_corrected$PPT_sum),'PPT_sum'] <- azer$mhat.p
plot(AAA$PPT_sum.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)

# correction pour le rayonnement
io = left_join(data_FON[,c('DATE','RG_sum')],RRR[,c('DATE','RG_sum')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$RG_sum),],RRR[,c('DATE','RG_sum')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$RG_sum.y)
data_FON_corrected[is.na(data_FON_corrected$RG_sum),'RG_sum'] <- azer$mhat.p
plot(AAA$RG_sum.y,azer$mhat.p)
 abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)

 
# correction pour le vent
io = left_join(data_FON[,c('DATE','WS_mean')],RRR[,c('DATE','WS_mean')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$WS_mean),],RRR[,c('DATE','WS_mean')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$WS_mean.y)
data_FON_corrected[is.na(data_FON_corrected$WS_mean),'WS_mean'] <- azer$mhat.p
plot(AAA$WS_mean.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)


# correction pour le RHmean
io = left_join(data_FON[,c('DATE','RHair_mean')],RRR[,c('DATE','RHair_mean')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$RHair_mean),],RRR[,c('DATE','RHair_mean')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$RHair_mean.y)
data_FON_corrected[is.na(data_FON_corrected$RHair_mean),'RHair_mean'] <- azer$mhat.p
plot(AAA$RHair_mean.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)


# correction pour le RHmin
io = left_join(data_FON[,c('DATE','RHair_min')],RRR[,c('DATE','RHair_min')],by='DATE')
colnames(io)=c('DATE','Site','SAFRAN')
io_NA = na.omit(io)
AAA = left_join(data_FON[is.na(data_FON$RHair_min),],RRR[,c('DATE','RHair_min')],by='DATE')
azer = QDM(o.c =  io_NA$Site,m.c = io_NA$SAFRAN,m.p=AAA$RHair_min.y)
data_FON_corrected[is.na(data_FON_corrected$RHair_min),'RHair_min'] <- azer$mhat.p
data_FON_corrected$RHair_min[data_FON_corrected$RHair_min<0] <- 0
data_FON_corrected$RHair_min[data_FON_corrected$RHair_min>100] <- 100
data_FON_corrected$RHair_min[data_FON_corrected$RHair_min>data_FON_corrected$RHair_mean] <- data_FON_corrected$RHair_mean[data_FON_corrected$RHair_min>data_FON_corrected$RHair_mean]
plot(AAA$RHair_min.y,azer$mhat.p)
abline(0,1,col=2)
plot(io$Site,io$SAFRAN)
abline(0,1,col=2)



# correction pour le RHmax from RHmean and RHmin only 
io = data_FON_corrected[is.na(data_FON_corrected$RHair_max),]
data_FON_corrected[is.na(data_FON_corrected$RHair_max),'RHair_max']=2*io$RHair_mean-io$RHair_min
sum(is.na(data_FON_corrected))

data_FON_corrected[,'Tair_mean'] <- round(data_FON_corrected[,'Tair_mean'],digit=2)
data_FON_corrected[,'Tair_min'] <- round(data_FON_corrected[,'Tair_min'],digit=2)
data_FON_corrected[,'Tair_max'] <- round(data_FON_corrected[,'Tair_max'],digit=2)
data_FON_corrected[,'WS_mean'] <- round(data_FON_corrected[,'WS_mean'],digit=2)
data_FON_corrected[,'RHair_mean'] <- round(data_FON_corrected[,'RHair_mean'],digit=2)
data_FON_corrected[,'RHair_min'] <- round(data_FON_corrected[,'RHair_min'],digit=2)
data_FON_corrected[,'RHair_max'] <- round(data_FON_corrected[,'RHair_max'],digit=2)
data_FON_corrected[,'PPT_sum'] <- round(data_FON_corrected[,'PPT_sum'],digit=2)
data_FON_corrected[,'RG_sum'] <- round(data_FON_corrected[,'RG_sum'],digit=2)


write.csv(data_FON_corrected,'data_FON_corrected.csv')

