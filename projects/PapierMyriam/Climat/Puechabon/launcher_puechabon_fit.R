# created on 09/08/2021
# author : julien ruffault  
# launcher to run SurEau-Ecos on Puechabon site and validation/comparison to data 


# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                                   # do not modify 

# set paths
climateData_path          <- paste0(mainDir,'/projects/Puechabon_fit/Climat_Puechabon_site.csv')
soilParameters_path       <- paste0(mainDir,'/projects/Puechabon_fit/Soil_Puechabon.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/Puechabon_fit/vegetation_Puechabon.csv')
output_path               <-  paste0(mainDir,'/projects/Puechabon_fit/test_Puechabon.csv')

# Create input files and run SurEau-Ecos
modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax =2.2, lat = 43.75, lon = 3.6)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path)
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)


vegetation_parameters$gsMax = 220
vegetation_parameters$gCrown0 = 150

vegetation_parameters$kPlantInit
vegetation_parameters$gsMax


vegetation_parameters$k_LSymInit

vegetation_parameters$k_RTInit

vegetation_parameters$Lv

vegetation_parameters$be

# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)


# Example output loading an plotting  ------------------------------------------
filename  = paste0(mainDir,"/projects/Puechabon_fit/test_Puechabon.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')



#plot(DATA$PPT,DATA$pptSoil)
#abline(0,1,col='red')
# # plot Psis
# plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-8,0),xlab='Time',ylab='Psi (MPa)')
# lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
# lines(DATA$Time,DATA$Psi_TSym,type='l',col='firebrick1',ylim=c(-6,0))
# 
# lines(DATA$Time,DATA$Psi_TApo,type='l',col='firebrick4')
# lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
# 
# 
# legend('bottomright',legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_Trunk_Symplasm','Psi_Trunk_Apoplasm','Psi_Soil'),
#        col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)
# 
# 
# io = month(DATA$Time)==6
# azer = aggregate(DATA$gs_lim[io],by=list(hour(DATA$Time[io])),mean)
# azer = aggregate(DATA$transpiration_mm[io],by=list(hour(DATA$Time[io])),mean)
# plot(azer)

# Loading potential dataset -------------------------------------------
data_potential = read.csv(paste0(mainDir,'/projects/Puechabon_fit/validation_data/Water_Potential_MIND_Control-1.csv'),dec=',',sep=';')  
data_potential = data_potential[data_potential$Treatment=='Control',]


data_PDpot = aggregate(data_potential$Pd_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_PDpot) <- c('Date','PDpot_measured')
data_PDpot$Date      <- as.Date(data_PDpot$Date,format='%d/%m/%Y')

data_MDpot = aggregate(data_potential$Md_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_MDpot) <- c('Date','MDpot_measured')
data_MDpot$Date <- as.Date(data_MDpot$Date,format='%d/%m/%Y')


DATA_day= aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$transpiration_mm = aggregate(DATA$transpiration_mm,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$evapotranspiration_mm = aggregate(DATA$transpiration_mm+DATA$SoilEvaporation_mm,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$PPT= aggregate(DATA$PPT,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
  

simu_DD=merge(DATA_day,data_PDpot,by='Date',all.x=T)
simu_DD=merge(simu_DD,data_MDpot,by='Date',all.x=T)

#quartz()
#plot(simu_DD$Psi_min,simu_DD$MDpot_measured,ylim=c(-8,0),xlim=c(-8,0),pch=16,col=2)
#points(simu_DD$Psi_base,simu_DD$PDpot_measured,ylim=c(-6,0),xlim=c(-6,0),pch=16,col=1)
#abline(0,1)


# sapflow dataset (2016-2018)---------------------------------------------------
library(readxl)
data_sapflow = as.data.frame(read_excel(paste0(mainDir,'/projects/Puechabon_fit/validation_data/Data_PUECH_LFMC_Paper/Sapflow_day_SMR_2016-2018.xlsx')))
data_sapflow$Date= as.Date(data_sapflow$Date,format='%Y-%m-%d')
colnames(data_sapflow)




simu_DD=merge(simu_DD,data_sapflow,by='Date',all.x=T)
#quartz()
#plot(simu_DD$transpiration_mm,simu_DD$E_Av.gap)
#abline(0,1,col=2)
#cor(simu_DD$transpiration_mm,simu_DD$E_Av.gap,use="complete.obs")^2


  for (year in 2016:2018){

  io =year(simu_DD$Date)==year
  quartz(width=8,height=10)
  par(mfrow=c(4,1),mar=c(2,2,2,2),oma=c(2,2,2,2))

  io =year(DATA_day$Date)==year

  plot(DATA_day$Date[io],DATA_day$Psi_min[io],type='l',col=2,ylim=c(-6,0),main=year,)
  lines(DATA_day$Date[io],DATA_day$Psi_base[io],type='l',col=1)
  points(data_MDpot$Date,data_MDpot$MDpot_measured,col='red',pch=16)
  points(data_PDpot$Date,data_PDpot$PDpot_measured,col='black',pch=16)
  par(new=T)
  barplot(DATA_day$PPT[io],col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,100))
  axis(4,col='blue',col.ticks='blue')
  grid()
  
  
  plot(simu_DD$E_Av.gap[io],simu_DD$transpiration_mm[io],main=year,pch=16,col=rgb(0.2,0.2,0.2,alpha=0.5),ylim=c(0,2.6),xlim=c(0,2.6))
  abline(0,1,col=2)
  lm_a =lm(simu_DD$E_Av.gap[io]  ~simu_DD$transpiration_mm[io])
  summary(lm_a)
  mtext(side=3, adj=1,paste0('R2 = ', round(summary(lm_a)$r.squared,digit=2)))
  grid()

  plot(simu_DD$Date[io],simu_DD$transpiration_mm[io],type='l',ylim=c(0,2.6))
  points(simu_DD$Date[io],simu_DD$E_Av.gap[io],col='forestgreen')
  grid()

  
  plot(simu_DD$Date[io],cumsum(simu_DD$transpiration_mm[io]),type='l',ylim=c(0,350))
  points(simu_DD$Date[io],cumsum(simu_DD$E_Av.gap[io]),col='forestgreen')
  grid()
  
  
  
}

io =year(DATA$Time)==2017
#quartz()

quartz(width=8,height=10)
par(mfrow=c(4,1),mar=c(2,2,2,2),oma=c(2,2,2,2))

plot(DATA_day$Date[year(DATA_day$Date)==2017],DATA_day$Psi_min[year(DATA_day$Date)==2017],type='l',col=2,ylim=c(-6,0),main=2017,)
lines(DATA_day$Date[year(DATA_day$Date)==2017],DATA_day$Psi_base[year(DATA_day$Date)==2017],type='l',col=1)
points(data_MDpot$Date,data_MDpot$MDpot_measured,col='red',pch=16)
points(data_PDpot$Date,data_PDpot$PDpot_measured,col='black',pch=16)
par(new=T)
barplot(DATA_day$PPT[year(DATA_day$Date)==2017],col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,100))
axis(4,col='blue',col.ticks='blue')
grid()


plot(DATA$fluxSoilToCollar1_mm[io],type='l',lwd=0.2)
lines(DATA$fluxSoilToCollar2_mm[io],type='l',col=2,lwd=0.2)
lines(DATA$fluxSoilToCollar3_mm[io],type='l',col=3,lwd=0.3)

plot(DATA$PsiSoil1[io],type='l',lwd=0.5)
lines(DATA$PsiSoil2[io],type='l',col=2,lwd=0.5)
lines(DATA$PsiSoil3[io],type='l',col=3,lwd=0.5)
#lines(DATA$Psi_AllSoil)
legend('bottomleft', col=c(1,2,3),c('layer1', 'layer2', 'layer3'),lty=1,cex=1.5)


# plot(DATA$PsiSoil1[io],type='l',lwd=0.5)
plot(DATA$REW1[io],type='l',lwd=0.5)
lines(DATA$REW2[io],type='l',col=2,lwd=0.5)
lines(DATA$REW3[io],type='l',col=3,lwd=0.5)
#lines(DATA$Psi_AllSoil)
legend('bottomleft', col=c(1,2,3),c('layer1', 'layer2', 'layer3'),lty=1,cex=1.5)


# 
# for (year in 2016:2018){
#   
#   io =year(simu_DD$Date)==year
#   quartz(width=6,height=5)
# 
#   plot(cumsum(simu_DD$transpiration_mm[io]),type='l',main=year,ylim=c(0,450)) 
#   grid()
# 
#   }
# 
# 
# 
# 
# 
# # sapflow dataset (2004-2009)---------------------------------------------------
# # data_sapflow = read.csv(paste0(mainDir,'/projects/Puechabon_fit/validation_data//SapflowPuechControl_2004-2009.csv'),sep=';',dec=',')
# # data_sapflow$Date= as.Date(data_sapflow$date,format='%d/%m/%Y')
# # io =year(data_sapflow$Date)==2005
# # data_sapflow=data_sapflow[!io,]
# # data_sapflow=data_sapflow[,c('Date','Tr.SMR.mm.day','Sd.Tr.SMR.mm.day')]
# # 
# # 
# # 
# # #plot(data_sapflow$Date,data_sapflow$Tr.SMR.mm.day,type='l')
# # #lines(DATA_day$Date,DATA_day$transpiration_mm,type='l',col='forestgreen')
# # 
# # simu_DD=merge(simu_DD,data_sapflow,by='Date',all.x=T)
# # 
# # plot(simu_DD$transpiration_mm,simu_DD$Tr.SMR.mm.day)
# # abline(0,1,col=2)
# # 
# # cor(simu_DD$transpiration_mm,simu_DD$Tr.SMR.mm.day,use="complete.obs")^2
# 
# 
# 
# #for (year in c(2004,2006,2007,2008,2009)){
# #   for (year in 2007){  
# #   
# #   io =year(simu_DD$Date)==year
# #   quartz(width=6,height=5)
# #   par(mfrow=c(2,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
# #   
# #   plot(simu_DD$transpiration_mm[io],simu_DD$Tr.SMR.mm.day[io],main=year)
# #   abline(0,1,col=2)
# #   
# #   lm_a =lm(simu_DD$Tr.SMR.mm.day[io]  ~simu_DD$transpiration_mm[io])
# #   summary(lm_a)
# #   mtext(side=3, paste0('R2 = ', round(summary(lm_a)$r.squared,digit=2)))
# #   plot(simu_DD$Date[io],simu_DD$transpiration_mm[io],type='l')
# #   lines(simu_DD$Date[io],simu_DD$Tr.SMR.mm.day[io],col='green')
# #   
# #   
# # }
# 
# 
# 
# # exploration comparaison avec les flux fluxnet 
# library(data.table)
# fluxes_tower = fread(paste0(mainDir,'/projects/Puechabon_fit/validation_data/FLX_FR-Pue_FLUXNET2015_FULLSET_DD_2000-2014_2-4.csv'))
# fluxes_tower$Date = as.Date(as.character(fluxes_tower$TIMESTAMP),format=c('%Y%m%d'))
# fluxes_tower= fluxes_tower[,c("Date","P_F","WS_F","NETRAD","TA_F_MDS","VPD_F_MDS","VPD_F_MDS_QC","LE_F_MDS","LE_F_MDS_QC")]
# nrow(fluxes_tower)
# 
# # on vire lannee 2005 avec les chenilles 
# 
# io =year(fluxes_tower$Date)==2005
# dfluxes_tower=fluxes_tower[!io,]
# 
# 
# 
# # 1. Virer les jours ou il pleut et le lendemain   (toutes le donnes) #
# io = aggregate(fluxes_tower$P_F, by = list(Date=as.factor(fluxes_tower$Date)) , FUN=sum)
# io1 = io[io$x>0.5,]
# selec =which(fluxes_tower$Date %in% as.Date(io1$Date)) # jours de pluie...
# selec_plus_1 =which(fluxes_tower$Date %in% (as.Date(io1$Date)+1)) # lendemain de pluie...
# c(selec,selec_plus_1)
# 
# io = vector(mode="logical",length = length(fluxes_tower$P_F))
# io[c(selec,selec_plus_1)]=T
# 
# fluxes_tower2 =fluxes_tower[!io,]
# dim(fluxes_tower2)  # <- 45224   231
# 
# #  .2. VENT : Virer les datas pour WS< 1m/S  ##
# fluxes_tower3=fluxes_tower2[fluxes_tower2$WS_F>1,]
# dim(fluxes_tower3) #  <- 43096   231
# 
# #  3. QC==1 pour LE
# fluxes_tower4= fluxes_tower3[fluxes_tower3$LE_F_MDS_QC==1,]
# dim(fluxes_tower4)
# 
# 
# io =fluxes_tower4[,c('Date','LE_F_MDS')]
# io[,2]=io[,2]*1800*18/10^6
# 
# simu_DD=merge(simu_DD,io,by='Date',all.x=T)
# colnames(simu_DD)
# cor(simu_DD$LE_F_MDS,simu_DD$evapotranspiration_mm, use="pairwise.complete.obs")^2
# plot(simu_DD$evapotranspiration_mm,simu_DD$LE_F_MDS,pch=1,col='grey30')
# abline(0,1,col='red',lwd=2)
# aaa=lm(simu_DD$LE_F_MDS~simu_DD$evapotranspiration_mm)
# plot(aaa)
# plot(simu_DD$Date,simu_DD$evapotranspiration_mm,col=1, type="l")
# points(simu_DD$Date,simu_DD$LE_F_MDS,type='p', col=2)
# 
# 
# 
# 
# 
# for (year in c(2004,2006,2007,2008,2009)){
#   
#   io =year(simu_DD$Date)==year
#   quartz(width=6,height=5)
#   par(mfrow=c(2,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
#   
#   plot(simu_DD$transpiration_mm[io],simu_DD$Tr.SMR.mm.day[io],main=year)
#   abline(0,1,col=2)
#   
#   lm_a =lm(simu_DD$Tr.SMR.mm.day[io]  ~simu_DD$transpiration_mm[io])
#   summary(lm_a)
#   mtext(side=3, paste0('R2 = ', round(summary(lm_a)$r.squared,digit=2)))
#   plot(simu_DD$Date[io],simu_DD$transpiration_mm[io],type='l')
#   lines(simu_DD$Date[io],simu_DD$Tr.SMR.mm.day[io],col='green')
#   
#   
# }
# 
# 
# 
# 
# 
# # exploration comparaison avec les flux fluxnet demi heure 
# library(data.table)
# fluxes_tower = fread(paste0(mainDir,'/projects/Puechabon_fit/validation_data/FLX_FR-Pue_FLUXNET2015_FULLSET_HH_2000-2014_2-4.csv'))
# fluxes_tower$Date = as.POSIXct(as.character(fluxes_tower$TIMESTAMP_END),format='%Y%m%d%H%M')
# fluxes_tower= fluxes_tower[,c("Date","P_F","WS_F","NETRAD","TA_F_MDS","VPD_F_MDS","VPD_F_MDS_QC","LE_F_MDS","LE_F_MDS_QC")]
# head(fluxes_tower)
# nrow(fluxes_tower)
# 
# 
# # on moyenne les flux à l'heure pour correspondre aux outputs de surEau
# 
# head(fluxes_tower$Date)
# head(fluxes_tower$Date+1800)
# head(hour(fluxes_tower$Date+1800))
# head(hour(fluxes_tower$Date))
# 
# 
# #azer =list(hour=hour(fluxes_tower$Date-1800),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date+5400),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date+3600),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date+1800),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# azer =list(hour=hour(fluxes_tower$Date-1800),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date-3600),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date-5400),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# #azer =list(hour=hour(fluxes_tower$Date),doy = yday(fluxes_tower$Date),year=year(fluxes_tower$Date))
# 
# io =aggregate(fluxes_tower$LE_F_MDS,by=azer,mean)
# colnames(io) = c("hour", "doy",  "year", "LE_F_MDS")
# io$P_F = aggregate(fluxes_tower$P_F,by=azer,sum)$x
# io$WS_F = aggregate(fluxes_tower$WS_F,by=azer,mean)$x
# io$LE_F_MDS_QC = aggregate(fluxes_tower$LE_F_MDS_QC,by=azer,max)$x
# 
# 
# io$Date=  as.POSIXct(paste(io$year,io$doy,io$hour,sep='/'),format='%Y/%j/%H')
# 
# 
# fluxes_tower_hour= io
# head(fluxes_tower_hour)
# dim(fluxes_tower_hour)
# 
# # 1. Virer les jours ou il pleut et le lendemain   (toutes le donnes) #
# io = aggregate(fluxes_tower_hour$P_F, by = list(Date=as.factor(as.Date(fluxes_tower_hour$Date))) , FUN=sum)
# io1 = io[io$x>0.5,]
# selec =which(as.Date(fluxes_tower_hour$Date) %in% as.Date(io1$Date)) # jours de pluie...
# selec_plus_1 =which(as.Date(fluxes_tower_hour$Date) %in% (as.Date(io1$Date)+1)) # lendemain de pluie...
# c(selec,selec_plus_1)
# 
# io = vector(mode="logical",length = length(fluxes_tower_hour$P_F))
# io[c(selec,selec_plus_1)]=T
# 
# fluxes_tower_hour_2 =fluxes_tower_hour[!io,]
# dim(fluxes_tower_hour_2)  # <- 45224   231
# 
# #  .2. VENT : Virer les datas pour WS< 1m/S  ##
# fluxes_tower_hour_3=fluxes_tower_hour_2[fluxes_tower_hour_2$WS_F>1,]
# dim(fluxes_tower_hour_3) #  <- 43096   231
# 
# #  3. QC==1 pour LE
# fluxes_tower_hour_4= fluxes_tower_hour_3[fluxes_tower_hour_3$LE_F_MDS_QC %in% c(0,1),]
# dim(fluxes_tower_hour_4)
# 
# 
# fluxes_tower_hour_5 = fluxes_tower_hour_4[fluxes_tower_hour_4$LE_F_MDS >0,]
# dim(fluxes_tower_hour_5)
# 
# 
# 
# FTH =fluxes_tower_hour_5
# 
# 
# 
# #A = io[io$year==2004,]
# #A$Date=  as.POSIXct(paste(A$year,A$doy,A$hour,sep='/'),format='%Y/%j/%H')
# #head(A$Date)
# #A$x = A$x*1e-3*1800/2257
# 
# #plot(FTH$Date,FTH$LE_F_MDS)
# #plot(FTH$Date[1:200],FTH$LE_F_MDS[1:200],type='l',panel.first=grid(nx=20))
# #lines(DATA$Time,DATA$transpiration_mm,col='blue')
# DATA$Date = DATA$Time
# 
# AAA = merge(FTH,DATA[,c('Date','transpiration_mm','SoilEvaporation_mm')],by='Date',all.x=T)
# head(AAA)
# # plot(AAA$transpiration_mm+AAA$SoilEvaporation_mm,AAA$LE_F_MDS*1e-3*1800/2257)
# # abline(0,1,col=2)
# # cor(AAA$x,AAA$transpiration_mm)
# 
# ######
# AAA$evapotranspiration  = AAA$transpiration_mm+AAA$SoilEvaporation_mm
# plot(AAA$evapotranspiration,AAA$LE_F_MDS*2*1e-3*1800/2257,cex=0.5)
# 
# abline(0,1,col=2)
# cor(AAA$LE_F_MDS*1e-3*1800/2257,AAA$transpiration_mm+AAA$SoilEvaporation_mm,use='complete.obs')^2
# 
# 
# # 
# # 
# # 
# # plot(AAA$LE_F_MDS[10000:10500]*1e-3*1800/2257,type='l' ,ylim=c(0,0.2))
# # lines(AAA$evapotranspiration[10000:10500],col=3 )
# # DATA$RH
# # 
# # io = (month(AAA$Date) %in% c(7:8)) &  year(AAA$Date)==2008
# # plot(AAA$LE_F_MDS[io]*1e-3*1800/2257,type='l' ,ylim=c(0,0.2))
# # lines(AAA$evapotranspiration[io],col=3 )
# # 
# # 
# # io = (month(AAA$Date) %in% c(11:12)) &  year(AAA$Date)==2008
# # plot(AAA$LE_F_MDS[io]*2*1e-3*1800/2257,type='l' ,ylim=c(0,0.2))
# # lines(AAA$evapotranspiration[io],col=3 )
# # 
# 
# io = (month(AAA$Date) %in% c(11,12,1,2))  &  (year(AAA$Date)%in% c(2008:2015))
# 
# azer= aggregate(AAA[io,"evapotranspiration"],by=list(AAA[io,"hour"]),mean)
# azer2= aggregate(AAA[io,"LE_F_MDS"],by=list(AAA[io,"hour"]),mean)
# 
# 
# io = (month(AAA$Date) %in% c(7,8))  &  (year(AAA$Date)%in% c(2008:2015))
# 
# azer3= aggregate(AAA[io,"evapotranspiration"],by=list(AAA[io,"hour"]),mean)
# azer4= aggregate(AAA[io,"LE_F_MDS"],by=list(AAA[io,"hour"]),mean)
# 
# 
# io = (month(AAA$Date) %in% c(3,4,5))  &  (year(AAA$Date)%in% c(2008:2015))
# azer5= aggregate(AAA[io,"evapotranspiration"],by=list(AAA[io,"hour"]),mean)
# azer6= aggregate(AAA[io,"LE_F_MDS"],by=list(AAA[io,"hour"]),mean)
# 
# io = (month(AAA$Date) %in% c(4,5,6))  &  (year(AAA$Date)%in% c(2008:2015))
# azer7= aggregate(AAA[io,"evapotranspiration"],by=list(AAA[io,"hour"]),mean)
# azer8= aggregate(AAA[io,"LE_F_MDS"],by=list(AAA[io,"hour"]),mean)
# 
# 
# par(mfrow=c(2,2),mar=c(2,2,1,1))
# plot(azer,type='l')
# lines(azer2$x*2*1e-3*1800/2257,col='green')
# 
# plot(azer3,type='l')
# lines(azer4$x*2*1e-3*1800/2257,col='green')
# 
# plot(azer5,type='l')
# lines(azer6$x*2*1e-3*1800/2257,col='green')
# 
# plot(azer7,type='l')
# lines(azer8$x*2*1e-3*1800/2257,col='green')
# 
# 
# 
# 
# io = (month(AAA$Date) %in% c(5))  &  (year(AAA$Date)%in% c(2008:2015))
# azer7= aggregate(AAA[io,"transpiration_mm"],by=list(AAA[io,"hour"]),mean)
# azer8= aggregate(AAA[io,"LE_F_MDS"],by=list(AAA[io,"hour"]),mean)
# plot(azer7)
# 
# 
# 
# 
