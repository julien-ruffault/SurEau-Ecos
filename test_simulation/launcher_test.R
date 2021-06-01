# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
# Test Launcher to run SurEau-ECOS-v1.0.0 on Champenoux
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

# climate data 
climateData_path          <- paste0(mainDir,'/test_simulation/Climat_constant_test.csv') # <-- indicate here the path to input climate data 

# Vegetation and soil parameters
soilParameters_path       <- paste0(mainDir,'/test_simulation/Soil_test.csv')
vegetationParameters_path <- paste0(mainDir,'/test_simulation/Parameters_test_quercus_evergreen.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  

# output
output_path               <- paste0(mainDir,'/test_simulation/test.csv')        

# create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                thresholdMortality = 99,
                                                numericalScheme = 'Implicit',
                                                defoliation = F,
                                                resetSWC=T)       

simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                      startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

climate_data     <- create.climate.data(filePath = climateData_path, 
                                        modeling_options = modeling_options,
                                        simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters  <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.373333 ,0.746666,1.119)) 
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)


# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
        simulation_parameters = simulation_parameters, 
       climate_data = climate_data,
       stand_parameters = stand_parameters, 
       soil_parameters = soil_parameters,
       vegetation_parameters = vegetation_parameters)


# output  Analysis -------------------------------------------------------------


# # for analyses / daily time scales 
# filename  = paste0(mainDir,"/Results_model/test.csv")
# DATA = read.csv(filename,header=T, dec='.',sep="")
# plot(DATA$daily_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# lines(DATA$daily_Psi_LApoMin,type='l',col='firebrick4')
# plot(DATA$daily_Psi_TSymMin)
# plot(DATA$daily_Psi_TApoMin)
# plot(DATA$daily_evaporation_mm)
# plot(DATA$daily_transpiration_mm)
# plot(DATA$daily_PLC_Root_max)
# plot(DATA$daily_PLC_TL_max)


# # for analyses / yearly time scales 
# filename  = paste0(mainDir,"/Results_model/test.csv")
# DATA = read.csv(filename,header=T, dec='.',sep="")
#  print(DATA)
# plot(DATA$yearly_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# lines(DATA$yearly_Psi_LApoMin,type='l',col='firebrick4')
# plot(DATA$yearly_evaporation_mm)
# plot(DATA$yearly_transpiration_mm)


# for analyses / subdaily time scales 
   filename  = paste0(mainDir,"/test_simulation/test.csv")
   DATA = read.csv(filename,header=T, dec='.',sep="")
   DATA$DD= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")
   plot(DATA$DD,DATA$Psi_LSym,type='l',col='forestgreen',ylim=c(-6,0))
   lines(DATA$DD,DATA$Psi_LApo,type='l',col='darkgreen')

   lines(DATA$DD,DATA$Psi_TSym,type='l',col='firebrick1',ylim=c(-6,0))
   lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
   
   lines(DATA$DD,DATA$Psi_AllSoil,col='grey20',lwd=2)
   
   plot(DATA$DD,DATA$PsiSoil1,type='l')
   