# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
#  Launcher to run tests on  SurEau-ECOS-v1.0.x 
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

# climate data 
climateData_path          <- paste0(mainDir,'/for_development/Climat_constant_test.csv') # <-- indicate here the path to input climate data 


# Vegetation and soil parameters
soilParameters_path       <- paste0(mainDir,'/for_development/Soil_test.csv')
vegetationParameters_path <- paste0(mainDir,'/for_development/Parameters_test_quercus_evergreen.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  



################################################################################
################################################################################
################################################################################
#                                 GRANIER   Priestley-Taylor                  #
################################################################################
################################################################################
################################################################################

# output
output_path               <- paste0(mainDir,'/for_development/test_granier.csv')


# create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             stomatalRegFormulation = "Sigmoid",
                                             transpirationModel = 'Granier',
                                             thresholdMortality = 99,
                                             numericalScheme = 'Semi-Implicit',
                                             defoliation = F,
                                             resetSWC=T)       

simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                      startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.35 ,0.7,1.05)) 
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


################################################################################
################################################################################
################################################################################
#                                 GRANIER    penman                           #
################################################################################
################################################################################
################################################################################

# output
output_path               <- paste0(mainDir,'/for_development/test_granier_penman.csv')


# create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             stomatalRegFormulation = "Sigmoid",
                                             transpirationModel = 'Granier',
                                             ETPFormulation = "Penman",
                                             thresholdMortality = 99,
                                             numericalScheme = 'Semi-Implicit',
                                             defoliation = F,
                                             resetSWC=T)       

simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                      startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.35 ,0.7,1.05)) 
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




################################################################################
################################################################################
################################################################################
#                                JARVIS                                       #
################################################################################
################################################################################
################################################################################


# output
output_path               <- paste0(mainDir,'/for_development/test_Jarvis.csv')


# create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             stomatalRegFormulation = "Sigmoid",
                                             transpirationModel = 'Jarvis',
                                             thresholdMortality = 99,
                                             numericalScheme = 'Semi-Implicit',
                                             defoliation = F,
                                             resetSWC=T)       

simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                      startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.35 ,0.7,1.05)) 
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





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# 
filename  = paste0(mainDir,"/for_development/test_Granier.csv")
DATA_granier = read.csv(filename,header=T, dec='.',sep="")
DATA_granier$Time = as.POSIXct(DATA_granier$Time,format='%Y-%m-%d/%H:%M:%S')


filename  = paste0(mainDir,"/for_development/test_Jarvis.csv")
DATA_jarvis = read.csv(filename,header=T, dec='.',sep="")
DATA_jarvis$Time = as.POSIXct(DATA_jarvis$Time,format='%Y-%m-%d/%H:%M:%S')


filename  = paste0(mainDir,"/for_development/test_Granier_penman.csv")
DATA_granier_penman = read.csv(filename,header=T, dec='.',sep="")
DATA_granier_penman$Time = as.POSIXct(DATA_granier_penman$Time,format='%Y-%m-%d/%H:%M:%S')


plot(DATA_granier$Time,DATA_granier$Psi_LSym,type='l')
lines(DATA_jarvis$Time,DATA_jarvis$Psi_LSym,type='l',col=2)
lines(DATA_granier_penman$Time,DATA_granier_penman$Psi_LSym,col=3)


plot(DATA_granier$transpiration_mm,type='l')
lines(DATA_jarvis$transpiration_mm,type='l',col=2)
lines(DATA_granier_penman$transpiration_mm,type='l',col=3)

plot(DATA_granier$Ebound,type='l')
lines(DATA_jarvis$Ebound,type='l',col=2)

plot(DATA_granier$Emin,type='l',ylim=c(0,0.3))
lines(DATA_jarvis$Emin,type='l',col=2)

plot(DATA_granier$leafTemperature,type='l')
lines(DATA_jarvis$leafTemperature,type='l',col=2)
lines(DATA_granier_penman$leafTemperature,type='l',col=3)


plot(DATA_granier$leafTemperature[1:48])
plot(DATA_granier$transpiration_mm[1:24])



# plot Psis
plot(DATA$DD,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-6,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$DD,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$DD,DATA$Psi_TSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
lines(DATA$DD,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('topright',legend=c('Psi_Lsym','Psi_Lapo','Psi_Tsym','Psi_Tapo','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)

