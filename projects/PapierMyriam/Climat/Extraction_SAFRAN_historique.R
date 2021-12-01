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


getwd()
SAFRAN_DIR <- "/Users/nicomartin/Documents/github/SureauR/SurEau-Ecos/SurEau-Ecos/projects/PapierMyriam/Climat/"
listeSite <- read.csv(paste(SAFRAN_DIR, "3Site_FR_infos.csv", sep=""), sep=";")
source(paste(SAFRAN_DIR, "RHfromSHandT.R", sep=""))

for (i in 1:nrow(listeSite))
{
  print(paste0('computing SITE : ',listeSite$site[i]))
  
  io =read.table(file=paste0(SAFRAN_DIR,'/quotidiennes_1959_2020_maille_',listeSite$Numero.Safran[i],'.csv.gz'),sep=';',h=T)

  
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
  filename <-  paste0(SAFRAN_DIR, listeSite$site[i],"_SAFRAN_histo.csv")
  write.table(x=RRR,file=filename,sep=';',dec='.',row.names=F)
  
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


