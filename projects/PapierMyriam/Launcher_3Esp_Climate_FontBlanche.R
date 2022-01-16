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
climateData_path_Safran          <- paste0(mainDir,'/projects/PapierMyriam/Climat/FontBlanche/Font Blanche_SAFRAN_histo.csv')


soilParameters_path       <- paste0(mainDir,'/projects/PapierMyriam/Sol/Soil_Puechabon.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/PapierMyriam/Vegetation/VegetationParams_Qilex.csv')

output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_Qilex_PapierMyriam_Climat_FontBlanche.csv')
# output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_QPub_PapierMyriam.csv')
# output_path               <-  paste0(mainDir,'/projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv')




# create model input files --------------------------------------------------
modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                compOptionsForEvapo = 'Fast',
                                                numericalScheme = 'Implicit',
                                                constantClimate=F,
                                                stomatalRegFormulation = "Turgor",
                                                defoliation = F, 
                                                thresholdMortality =100,
                                                resetSWC=T,
                                                transpirationModel="Jarvis")     



simulation_parameters <- create.simulation.parameters(startYearSimulation = 2010,                        
                                                      endYearSimulation = 2018,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'simple_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #

stand_parameters_FontBlanche <- create.stand.parameters(LAImax = 2. , lat = 43.74, lon = 3.59)

soil_parameters  <- create.soil.parameters(filePath= soilParameters_path) 


TTT = read.vegetation.file(filePath=vegetationParameters_path ,modeling_options=modeling_options)
vegetation_parameters <- create.vegetation.parameters(listOfParameters=TTT, stand_parameters = stand_parameters_FontBlanche, soil_parameter = soil_parameters, modeling_options = modeling_options)


#On mets le gmin Tronc à 0 pour limiter les interférence avec gmin root+Trunk+Branch
vegetation_parameters$vol_Stem <- 39.9 # 18.2 #/20
vegetation_parameters$symFrac_Stem <- 0.213 #0.0917 # # # #0.0917 #0.213 # 0.0917 #  0.0975 #Trunk 0.0917
vegetation_parameters$symFrac_Stem * vegetation_parameters$vol_Stem


# Run SurEau-Ecos ---------------------------------------------------------

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters_FontBlanche, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

filenamePhClimFB  = paste0(mainDir,"/Projects/PapierMyriam/Output_Qilex_PapierMyriam_Climat_FontBlanche.csv")
# filenameQPub  = paste0(mainDir,"/Projects/PapierMyriam/Output_QPub_PapierMyriam.csv")
# filenamePHal  = paste0(mainDir,"/Projects/PapierMyriam/Output_Phalepensis_PapierMyriam.csv")

DATAPh = read.csv(filenamePhClimFB,header=T, dec='.', sep="")
plot(DATAPh$Psi_LApo, type='l', col="dark green", lwd=3)
