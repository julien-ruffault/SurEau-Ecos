# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Example launcher to run SurEau-Ecos

# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##


# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory
# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))                # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 
library(data.table)
library(R.utils)

fread("/Users/nicomartin/Documents/github/SureauR/SurEau-Ecos/SurEau-Ecos/projects/PapierMyriam/quotidiennes_1959_2019_maille_8427.csv.gz")

#--------------------------------------------------------------------------------
# set paths
climateData_path          <- paste0(mainDir,'/projects/Forgenius/ERA_europe_0.37S-46.13E.csv')
soilParameters_path       <- paste0(mainDir,'/projects/Rare-Root/soil_Rustrel.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/Rare-Root/vegetation_Rustrel.csv')
output_path               <-  paste0(mainDir,'/projects/Forgenius/Rustrel_output_subdaily.csv')




#----------------------------------------------------
# Create input files and run SurEau-Ecos

#Option de modélisation : SurEau-Ecos ne pas toucher
modeling_options      <- create.modeling.options()  
#Paramètres de simulations de SurEAu-Ecos: ne toucher que les années
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2020,                       
                                                      endYearSimulation = 2020,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


#Attention dans le fichier climat il manque des pluies en 2003 au moins.
climate_data          <- create.climate.data(filePath = climateData_path, 
                                        modeling_options = modeling_options,
                                        simulation_parameters = simulation_parameters) #

#tapply(climate_data$PPT_sum, climate_data$YEAR, sum, na.rm=F)

#Parametre de stand à régler ici en dur
stand_parameters      <- create.stand.parameters(LAImax = 2, lat = 43.00, lon = 4.00)

#Soil parameters : lu depuis le fichier mais peut être modifié en dur
TTTSoil = read.soil.file(soilParameters_path)
#Fraction de cailloux par couche
RFC = c(TTTSoil$RFC_1,TTTSoil$RFC_2,TTTSoil$RFC_3)
depths = c(TTTSoil$depth1,TTTSoil$depth2,TTTSoil$depth3)
#Puis création de l'objet
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path, listOfParameters = TTTSoil) 



#Plant all parameters : ici on lit depuis le ficheir pour du chêne vert
TTTPlant=read.vegetation.file(vegetationParameters_path, modeling_options = modeling_options)

#----
# les paramètres de racines  sensibles pour notre étude
#Root distribution: paramètre qui gère la disctribiuton de racines (entre 0.9 et 1)
TTTPlant$betaRootProfile <- 0.97 #0.96
#Root to leaf area : fraction qui gère la quantitié de racine (= Lv total)
TTTPlant$fRootToLeaf <- 1
#Diamètre des racines fines (m)
TTTPlant$rootRadius <- 2e-04
#Puis on créé l'objet paramètes de plantes (attention requiert les paramètres de sol !!)
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path,
                                                      listOfParameters = TTTPlant,
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)


#Plot les paramètres calculé avec la distribution racinaire et le sol
par(mfrow=c(2,3), pty='s')
DepthLayers=soil_parameters$depth*100
depthvec = seq(0, 4, 0.1)
fractionofRoot = 1-TTTPlant$betaRootProfile^(depthvec*100)
plot(fractionofRoot, depthvec*100, type='l', ylim=c(400,0), xlim=c(1,0), ylab="depth (cm)", xlab="root fraction")
abline(h=DepthLayers)
barplot(vegetation_parameters$rootDistribution, ylab='root fraction per layer')
barplot(soil_parameters$V_field_capacity, ylab="Vol. field capacity")
barplot(vegetation_parameters$K_RSApoInit, ylab="soil-root conductance per layer")

curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Leaf,P50=vegetation_parameters$P50_VC_Leaf),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", xlab="Psi")
curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Stem,P50=vegetation_parameters$P50_VC_Stem),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", lty=3, add=T, col=2, lwd=2)
par(new=T)
curve(GsCurve(x, slope=vegetation_parameters$slope_gs,P50=vegetation_parameters$P50_gs, gsmax=vegetation_parameters$gsMax),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , col=4, yaxt="n", ylab="",xlab="")
axis(4)
mtext("gs",4,1.8,cex=.8)
barplot(c(vegetation_parameters$K_PlantInit ,vegetation_parameters$LAImax,vegetation_parameters$gmin20), names.arg = c("Kplant", "LAI", "gmin"))


#barplot(vegetation_parameters$K_RSApoInit/sum(vegetation_parameters$K_RSApoInit)*100, ylab="relative soil-root conductance")



# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)



# Example output loading an plotting  ------------------------------------------
filename  = paste0(mainDir,"/projects/Forgenius/Rustrel_output_subdaily.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

# plot Psis
quartz()
par(new=F, mfrow=c(1,1))
plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-5,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$Time,DATA$Psi_SSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$Time,DATA$Psi_SApo,type='l',col='firebrick4')
lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
par(new=T)
plot(DATA$Time,DATA$PPT,type='h', col='black',xlab='',ylab='',yaxt="n")
axis(4)
legend('bottomright',legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_TrunK_Symplasm','Psi_TrunK_Apoplasm','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.5)

#Water extraction from the different layers
plot(DATA$fluxSoilToStem1_mm, type='l', ylim=c(-0.1,0.2))
lines(DATA$fluxSoilToStem2_mm, type='l', col=2)
lines(DATA$fluxSoilToStem3_mm, type='l', col=4)
legend('bottomleft',legend=c('first layer','second layer','third layer'),
       col=c(1,2,4),lty=1,lwd=1,cex=0.8)


tail(DATA$gcanopy_lim)
tail(DATA$leafVPD)
tail(DATA$VPD)
tail(DATA$Elim)
tail(DATA$gmin)
tail(DATA$Emin)



# plot meteorological conditions 
plot(DATA$Time,DATA$Tair,type='l',col='firebrick4',ylab='Air temperature (degC)', xlab='Time')
par(new=T)
barplot(DATA$PPT,col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,60))
axis(4,col='blue',col.ticks='blue')


# plot water fluxes 
plot(DATA$Time,DATA$transpiration_mm,type='l',col='blue',xlab='Time',ylab='water fluxes (mm/timestep)')
lines(DATA$Time,DATA$Emin_mm,col='forestgreen')
lines(DATA$Time,DATA$Emin_S_mm,col='brown4')
lines(DATA$Time,DATA$soilEvaporation_mm,type='l',col='grey30')
legend('topright',legend=c('Transpiration','Emin','Emin_S','Soil'),
       col=c('blue','forestgreen','brown4','grey30'),lty=1,lwd=2,cex=0.8)

# plot cavitation 
plot(DATA$Time,DATA$PLC_Leaf,type='l', col='springgreen4',ylim=c(0,70),xlab='Time',ylab='PLC')
lines(DATA$Time,DATA$PLC_Stem,type='l',col='brown')
legend('topleft',legend=c('PLC_Leaf','PLC_Stem'),
       col=c('springgreen4','brown'),lty=1,lwd=2,cex=0.8)
