# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Example launcher to run SurEau-Ecos

# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##


# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory
# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))                # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

#--------------------------------------------------------------------------------
# set paths
climateData_path          <- paste0(mainDir,'/projects/PapierMyriam/Climat_constant.csv')
soilParameters_path       <- paste0(mainDir,'/projects/PapierMyriam/Soil_Papier3Esp.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/PapierMyriam/VegetationParams_Papier3Especes.csv')

output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_QPub_PapierMyriam.csv')
output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv')




# create model input files --------------------------------------------------
modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                compOptionsForEvapo = 'Normal',
                                                numericalScheme = 'Implicit',
                                                constantClimate = T,
                                                stomatalRegFormulation = "Turgor",
                                                defoliation = F, 
                                                thresholdMortality = 100,
                                                resetSWC = T,
                                                transpirationModel="Jarvis")     



simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

simulation_parameters$compOptionsForEvapo
climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters  <- create.soil.parameters(filePath=soilParameters_path) 



vegetation_parameters <- create.vegetation.parameters(filePath=vegetationParameters_path, stand_parameters = stand_parameters, soil_parameter = soil_parameters, modeling_options = modeling_options)

vegetation_parameters$Foliage

#On mets le gmin Tronc à 0 pour limiter les interférence avec gmin root+Trunk+Branch
vegetation_parameters$vol_Stem <- 39.9 # 18.2 #/20
vegetation_parameters$symFrac_Stem <- 0.213 #0.0917 # # # #0.0917 #0.213 # 0.0917 #  0.0975 #Trunk 0.0917
vegetation_parameters$symFrac_Stem * vegetation_parameters$vol_Stem


# Run SurEau-Ecos ---------------------------------------------------------

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

filename  = paste0(mainDir,"/Projects/PapierMyriam/Output_PapierMyriam.csv")
filenameQPub  = paste0(mainDir,"/Projects/PapierMyriam/Output_QPub_PapierMyriam.csv")
filenamePHal  = paste0(mainDir,"/Projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv")

filename  = paste0(mainDir,"/Projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv")

DATA = read.csv(filename,header=T, dec='.', sep="")
DATAQPub = read.csv(filenameQPub, header=T, dec='.',sep="")
DATAPHal = read.csv(filenamePHal, header=T, dec='.',sep="")

quartz()
par(mfrow=c(2,1), pty="s")
plot(DATA$LAI, type='l', col="dark green", lwd=3)
par(new=T)
plot(DATA$transpiration_mm, type='l', col=4, yaxt="n", ylab="")
par(new=T)
plot(DATA$Psi_LApo, type='l', col=1, ylab="", yaxt="n")

SWS= DATA$SWS1+DATA$SWS2+DATA$SWS3
plot(DATA$gs_lim, type='l', col=1, ylab="gs")
par(new=T)
plot(SWS, type='l',  ylab="", yaxt="n", lwd=4, col=2)
axis(4)
par(new=T)
plot(DATA$Psi_LApo, type='l', col=4, ylab="", yaxt="n")



plot(DATA$PLC_Leaf, type='l', col="dark green", lwd=3)
lines(DATAPHal$PLC_Leaf, type='l', col="orange",lty=1, lwd=3)
lines(DATAQPub$PLC_Leaf, type='l', col="brown",lty=1, lwd=3)

lines(DATA$PLC_Leaf, type='l', col="dark green",lty=2, lwd=3)

