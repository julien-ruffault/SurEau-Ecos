# Script cree le 02/12/2020 par JR (julien.ruff@gmail.com)
# Extraction des donnes SAFRAN pour les sites Reseau ICOS foret + Champenoux  / pour validation ICOS 
# Mise en forme au format d'entree SUREAU_ECOS pour être diretement intégrés dans le modele
# Sauvegarde pour chaque site d'un fichier csv nommé "'NOMDESITE'_SAFRAN_histo.csv"


# DATE	      Tair_min	Tair_max	Tair_mean	RG_sum	PPT_sum	RHair_min	RHair_mean	RHair_max  WS
# DD/MM/YYYY  (°C)      (°C)      (°C)      (MJ/m2)     (mm)     (%)      (%)         (%)    (m/s)

#projection LIIE/ Code numerique est : 27572
#projection L93 / le Code numerique est : 2154
#Projection WGS84 / code numerique 4326

rm(list=ls())
gc()


library(readxl)
library(xlsx)
library(sp)
library(raster)
library(fields)
library(rgdal)
library(xlsx)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

SAFRAN_DIR <-  "/Volumes/paca-urfm/URFM/PEF/Transferts-TEMP/Safran"


SafranCoord<-read.table(paste0(SAFRAN_DIR,"/coord-grille-sim2-fev2018.csv"), sep=";", h=T)
SafranCoord[,c(4,5)] <- SafranCoord[,c(4,5)]*100  #1 attention multiplié par 100 car comme ca dans le fichier orignial 

# load info sites 
SITES_INFO= data.frame(read_excel('SITES_INFO.xlsx')) # fichier avec les coordonnes des sites etudies
SITES_INFO.sp <-  SITES_INFO
coordinates(SITES_INFO.sp) <- ~LON+LAT
crs(SITES_INFO.sp) =CRS("+init=EPSG:4326")

SITES_INFO.sp.LIIE = spTransform(SITES_INFO.sp,CRS("+init=EPSG:27572"))
SITES_INFO.sp.L93 = spTransform(SITES_INFO.sp,CRS("+init=EPSG:2154"))

# load contires
CTR = readOGR(dsn = 'ne_50m_admin_0_countries' ,layer= 'ne_50m_admin_0_countries')

plot(SITES_INFO.sp,col='grey30',bg='grey30',add=T,pch=21,
     xlim=c(0,5))

quartz(width=4,height=4)
par(mar=c(1,1,1,1))
plot(CTR,border='black',lwd=0.7,xlim=c(1,4),ylim=c(40,52))
box(lwd=0.7)
plot(SITES_INFO.sp,col='grey30',bg='grey30',add=T,pch=21,cex=0.8)

plot(SITES_INFO.sp[6,],col='forestgreen',bg='forestgreen',add=T,pch=21,cex=0.8)



readOG


SafranISBA=rasterFromXYZ(cbind(SafranCoord[,4],SafranCoord[,5],SafranCoord[,1])) #
plot(SafranISBA)
plot(SITES_INFO.sp.LIIE,col='red',add=T)
extract(SafranISBA,SITES_INFO.sp.LIIE)

SI_spatial <- cbind(SITES_INFO,NPIX_SAFRAN=extract(SafranISBA,SITES_INFO.sp.LIIE))



write.xlsx(SI_spatial,'SI_spatial.xlsx',row.names=F)

# puis creation de fichiers d'entree qui vont bien pour aller directement en model 
# il faut les variables suivantes : 

source('RHfromSHandT.R')


for (i in 1:nrow(SI_spatial))
{
  print(paste0('computing SITE : ',SI_spatial[i,'SITE']))
  io =read.table(file=paste0(SAFRAN_DIR,'/quotidiennes_1959_2019_maille_',SI_spatial$NPIX_SAFRAN[i],'.csv.gz'),sep=';',h=T)

  
  # DATE	      Tair_min	Tair_max	Tair_mean	RG_sum	PPT_sum	RHair_min	RHair_mean	RHair_max  WS
  # DD/MM/YYYY  (°C)      (°C)      (°C)      (MJ/m2)     (mm)     (%)      (%)         (%)    (m/s)
  
  RRR       <- data.frame(DATE       = io$date,
                          Tair_min   = pmin(io[, "tinf_h_q"],io[, "t_q" ]), ## Tmin(degC) /corrige pour les cas ou tmin> tmoy dans les donnees SAFRAN (ca arrive)
                          Tair_max   = pmax(io[, "tsup_h_q"],io[, "t_q" ]),# Tmax(degC) corrige pour les cas ou tmax< tmoy dans les donnees SAFRAN (ca arrive)
                          Tair_mean  = io[, "t_q" ],
                          RG_sum      = io[, "ssi_q"] / 100,
                          PPT_sum     = io[,"preliq_q"]+io[,"prenei_q"],## precipitation (mm)
                          RHair_min  = RHfromSHandT(Tc=pmax(io[, "tsup_h_q"],io[, "t_q" ]),HS = io[, "q_q"] /1000), 
                          RHair_max  = RHfromSHandT(Tc=pmin(io[, "tinf_h_q"],io[, "t_q" ]),HS = io[, "q_q"] /1000), 
                          RHair_mean = RHfromSHandT(Tc=io[, "t_q" ],HS = io[, "q_q"] /1000), 
                          WS_mean    = io[, "ff_q"]) ## Wind speed (m/s)
  # ecriture des fichiers  "'NOMDESITE'_SAFRAN_histo.csv"
  filename <-  paste0(SI_spatial$SITE[i],"_SAFRAN_histo.csv")
  write.csv2(x=RRR,file=filename,sep=';',dec=',',row.names=F)
  
}








# notes 
# 
# PPT  = io[,"preliq_q"]+io[,"prenei_q"]  ## precipitation (mm)
# Tmoy = io[, "t_q"] ## Tmoy (degC)
# Tmin = io[, "tinf_h_q"] ## Tmin(degC)
# Tmax = io [,"tsup_h_q"] ## Tmax(degC)
# WSmoy   = io[, "ff_q"] ## Wind speed (m/s)
# SHmoy = io[, "q_q"]  ## Specific humidity (initialement en kg/kg || converti en g/kg)
# RG = io[, "ssi_q"] / 100 ## global radiation (initialement en J/cm2 ||) conversionn en MJ/m2
# RHmoy = io[, "hu_q"] # relative humidity (%)
# 

# 
# RHbis = RHfromSHandT(Tc=Tmoy,HS = SHmoy/1000)
# RHmin = RHfromSHandT(Tc=Tmax,HS = SHmoy/1000)
# RHmax = RHfromSHandT(Tc=Tmin,HS = SHmoy/1000)
# 
# plot(RHmoy,RHmin)
# plot(RHmoy,RHmax)
# abline(0,1,col='red')
# 
# plot(RHmin,RHmax)
# abline(0,1,col='red')


