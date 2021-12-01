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
climateData_path          <- paste0(mainDir,'/projects/PapierMyriam/Climat/FontBlanche/data_FON_corrected.csv')

soilParameters_path       <- paste0(mainDir,'/projects/PapierMyriam/Sol/Soil_Puechabon.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/PapierMyriam/Vegetation/VegetationParams_Qilex.csv')

output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_Qilex_PapierMyriam_Climat_FontBlanche.csv')
# output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_QPub_PapierMyriam.csv')
# output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv')




# create model input files --------------------------------------------------
modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                compOptionsForEvapo = 'Normal',
                                                numericalScheme = 'Implicit',
                                                constantClimate=F,
                                                stomatalRegFormulation = "Turgor",
                                                defoliation = F, 
                                                thresholdMortality =100,
                                                resetSWC=T,
                                                transpirationModel="Jarvis")     



simulation_parameters <- create.simulation.parameters(startYearSimulation = 2010,                        
                                                      endYearSimulation = 2010,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'simple_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #

stand_parameters_Puechabon <- create.stand.parameters(LAImax = 2. , lat = 43.74, lon = 3.59)

soil_parameters  <- create.soil.parameters(filePath= soilParameters_path) 


TTT = read.vegetation.file(filePath=vegetationParameters_path ,modeling_options=modeling_options)
vegetation_parameters <- create.vegetation.parameters(listOfParameters=TTT, stand_parameters = stand_parameters, soil_parameter = soil_parameters, modeling_options = modeling_options)


#On mets le gmin Tronc à 0 pour limiter les interférence avec gmin root+Trunk+Branch
vegetation_parameters$VolumeLiving_TRB <- 39.9 # 18.2 #/20
vegetation_parameters$SymplasmicFrac_Trunk <- 0.213 #0.0917 # # # #0.0917 #0.213 # 0.0917 #  0.0975 #Trunk 0.0917
vegetation_parameters$SymplasmicFrac_Trunk * vegetation_parameters$VolumeLiving_TRB


# Run SurEau-Ecos ---------------------------------------------------------

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

filenameQilClimPuech  = paste0(mainDir,"/Projects/PapierMyriam/Output_Qilex_PapierMyriam_Climat_FontBlanche.csv")
# filenameQPub  = paste0(mainDir,"/Projects/PapierMyriam/Output_QPub_PapierMyriam.csv")
# filenamePHal  = paste0(mainDir,"/Projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv")

DATAQil = read.csv(filenameQilClimPuech,header=T, dec='.', sep="")
# DATAQPub = read.csv(filenameQPub, header=T, dec='.',sep="")
# DATAPHal = read.csv(filenamePHal, header=T, dec='.',sep="")


# plot(DATAQil$PLC_Leaf, type='l', col="dark green", lwd=3)
# lines(DATAPHal$PLC_Leaf, type='l', col="orange",lty=2, lwd=3)
# lines(DATAQPub$PLC_Leaf, type='l', col="brown",lty=2, lwd=3)
# plot(DATAQil$daily_Psi_LSymMax, type='l', col="dark blue")
# lines(DATAQil$daily_Psi_LSymMin, type='l', col="orange")

plot(DATAQil$Psi_LApo, type='l', col="dark green", lwd=3)
lines(DATAPHal$Psi_LApo, type='l', col="orange",lty=2, lwd=3)
lines(DATAQPub$Psi_LApo, type='l', col="brown",lty=2, lwd=3)

plot(DATAQil$Psi_LSym, type='l', col="dark green", lwd=3)
lines(DATAPHal$Psi_LSym, type='l', col="orange",lty=2, lwd=3)
lines(DATAQPub$Psi_LSym, type='l', col="brown",lty=2, lwd=3)
par(new=T)
plot(DATAQil$PLC_Leaf, type='l', col="dark green", lwd=3, yaxt="n", ylab="")
lines(DATAPHal$PLC_Leaf, type='l', col="orange",lty=2, lwd=3)
lines(DATAQPub$PLC_Leaf, type='l', col="brown",lty=2, lwd=3)

#lines(DATA$PLC_Leaf, type='l', col="dark green",lty=2, lwd=3)

