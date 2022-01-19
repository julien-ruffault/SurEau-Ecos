# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
#  Launcher to run tests on  SurEau-ECOS-v1.0.x 
#  test for explicit numericalScheme / parametrisation based on the comparison with Sureau.C
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory


SECONDS=2

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   
# en explicit   
# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

# climate data 
climateData_path          <- paste0(mainDir,'/projects/Compar_SurEauC/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 


# Vegetation and soil parameters
soilParameters_path       <- paste0(mainDir,'/projects/Compar_SurEauC/Soil_test_champenoux.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/Compar_SurEauC/VegetationParams_ComparSurEauC.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  

  # output
  output_path               <- paste0(mainDir,'/for_development/test_explicit_V2_cav_',SECONDS,'s.csv')        
  
  #create input lists to run SureauR --------------------------------------------
  modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                               constantClimate=T,
                                               stomatalRegFormulation = "Sigmoid",
                                               thresholdMortality = 99,
                                               numericalScheme = 'Explicit',
                                               compOptionsForEvapo = "Custom" ,
                                               Lcav=1,
                                               Scav=1,
                                               customSmallTimeStepInSec = SECONDS,
                                               defoliation = F,
                                               resetSWC=T)
  
  
  # modeling_options  <- create.modeling.options(timeStepForEvapo=1,
  #                                              constantClimate=T,
  #                                              stomatalRegFormulation = "Sigmoid",
  #                                              thresholdMortality = 99,
  #                                              numericalScheme = 'Implicit',
  #                                              defoliation = F,
  #                                              resetSWC=T)
  
  
  simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                        startYearSimulation = 1990,                        
                                                        endYearSimulation = 1990,
                                                        resolutionOutput = "subdaily",
                                                        outputType = 'simple_subdaily',
                                                        overWrite = T,
                                                        outputPath = output_path)
  
  
  climate_data          <- create.climate.data(filePath = climateData_path, 
                                               modeling_options = modeling_options,
                                               simulation_parameters = simulation_parameters) #
  stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
  soil_parameters       <- create.soil.parameters(filePath=soilParameters_path) 
  vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                        stand_parameters = stand_parameters, 
                                                        soil_parameter = soil_parameters,
                                                        modeling_options = modeling_options)
  # 
  # 
  vegetation_parameters$C_LApoInit  = 10000
  vegetation_parameters$C_SApoInit  = 10000
  
  
  # vegetation_parameters$C_LApoInit  = 1e-100
  # vegetation_parameters$C_SApoInit  = 1e-100
  
  # run SurEau-Ecos --------------------------------------------------------------
  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)


  
  
  
  
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   
  # en explicit   no cavitatioin 
  # ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  
  
  # output
  output_path               <- paste0(mainDir,'/for_development/test_explicit_V2_cav_SC',SECONDS,'s.csv')        
  
  
  # climate data 
  climateData_path          <- paste0(mainDir,'/projects/Compar_SurEauC/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
  
  
  # Vegetation and soil parameters
  soilParameters_path       <- paste0(mainDir,'/projects/Compar_SurEauC/Soil_test_champenoux.csv')
  vegetationParameters_path <- paste0(mainDir,'/projects/Compar_SurEauC/VegetationParams_ComparSurEauC.csv')
  #standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  
  

  #create input lists to run SureauR --------------------------------------------
  modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                               constantClimate=T,
                                               stomatalRegFormulation = "Sigmoid",
                                               thresholdMortality = 99,
                                               numericalScheme = 'Explicit',
                                               compOptionsForEvapo = "Custom" ,
                                               Lcav=0,
                                               Scav=0,
                                               customSmallTimeStepInSec = SECONDS,
                                               defoliation = F,
                                               resetSWC=T)
  
  
  # modeling_options  <- create.modeling.options(timeStepForEvapo=1,
  #                                              constantClimate=T,
  #                                              stomatalRegFormulation = "Sigmoid",
  #                                              thresholdMortality = 99,
  #                                              numericalScheme = 'Implicit',
  #                                              defoliation = F,
  #                                              resetSWC=T)
  
  
  simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                        startYearSimulation = 1990,                        
                                                        endYearSimulation = 1990,
                                                        resolutionOutput = "subdaily",
                                                        outputType = 'simple_subdaily',
                                                        overWrite = T,
                                                        outputPath = output_path)
  
  
  climate_data          <- create.climate.data(filePath = climateData_path, 
                                               modeling_options = modeling_options,
                                               simulation_parameters = simulation_parameters) #
  stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
  soil_parameters       <- create.soil.parameters(filePath=soilParameters_path) 
  vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                        stand_parameters = stand_parameters, 
                                                        soil_parameter = soil_parameters,
                                                        modeling_options = modeling_options)
  # 
  # 
  vegetation_parameters$C_LApoInit  = 10000
  vegetation_parameters$C_SApoInit  = 10000
  
  
  # vegetation_parameters$C_LApoInit  = 1e-100
  # vegetation_parameters$C_SApoInit  = 1e-100
  
  # run SurEau-Ecos --------------------------------------------------------------
  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)
  
  
  
  
  
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   
# en implicit   
# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
output_path               <- paste0(mainDir,'/for_development/test_implicit_V2_cav_',SECONDS,'s.csv')        
  

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
                                                      outputType = 'simple_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path) 
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)
# 
# 
vegetation_parameters$C_LApoInit  = 10000
vegetation_parameters$C_SApoInit  = 10000


# vegetation_parameters$C_LApoInit  = 1e-100
# vegetation_parameters$C_SApoInit  = 1e-100

# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)
  
  
  


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   
# en implicit   sans cavit 
# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
output_path               <- paste0(mainDir,'/for_development/test_implicit_V2_cav_SC',SECONDS,'s.csv')        


modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             stomatalRegFormulation = "Sigmoid",
                                             thresholdMortality = 99,
                                             Lcav=0,
                                             Scav=0,
                                             numericalScheme = 'Implicit',
                                             defoliation = F,
                                             resetSWC=T)


simulation_parameters <- create.simulation.parameters(mainDir = mainDir,
                                                      startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'simple_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path) 
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)
# 
# 
vegetation_parameters$C_LApoInit  = 10000
vegetation_parameters$C_SApoInit  = 10000


# vegetation_parameters$C_LApoInit  = 1e-100
# vegetation_parameters$C_SApoInit  = 1e-100

# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)





# plot comparaisonn 

filename  = paste0(mainDir,'/for_development/test_explicit_V2_cav_',SECONDS,'s.csv')               
io_Explicit= read.csv(filename,header=T, dec='.',sep="")
io_Explicit$DD= as.POSIXct(io_Explicit$Time,format='%Y-%m-%d/%H:%M:%S')

filename  = paste0(mainDir,'/for_development/test_explicit_V2_cav_SC',SECONDS,'s.csv')               
io_Explicit_SC= read.csv(filename,header=T, dec='.',sep="")
io_Explicit_SC$DD= as.POSIXct(io_Explicit_SC$Time,format='%Y-%m-%d/%H:%M:%S')


filename  = paste0(mainDir,'/for_development/test_implicit_V2_cav_',SECONDS,'s.csv')               
io_Implicit= read.csv(filename,header=T, dec='.',sep="")
io_Implicit$DD= as.POSIXct(io_Implicit$Time,format='%Y-%m-%d/%H:%M:%S')


filename  = paste0(mainDir,'/for_development/test_implicit_V2_cav_SC',SECONDS,'s.csv')               
io_Implicit_SC= read.csv(filename,header=T, dec='.',sep="")
io_Implicit_SC$DD= as.POSIXct(io_Implicit_SC$Time,format='%Y-%m-%d/%H:%M:%S')


quartz()
par(mfrow=c(2,2),mar=c(2,2,2,2))

plot(io_Implicit$Psi_LApo,col=1,type='l',lwd=2)
lines(io_Explicit$Psi_LApo,type='l',col=2,lty=2,lwd=2)
lines(io_Implicit_SC$Psi_LApo,col=3,lwd=2)
lines(io_Explicit_SC$Psi_LApo,type='l',col=4,lty=2,lwd=2)

polygon(x=c(0,50,50,0),y=c(-1,-1,0,0),lty=2)
polygon(x=c(450,500,500,450),y=c(-5,-5,-4,-4),lty=2)


legend('topright',lty=c(1,2,1,2),lwd=2,col=1:4,legend=c('Implicit', 'Explicit','Implicit_NoCavit','Explicit_NoCavit'))


plot(io_Implicit$Psi_LApo,col=1,type='l',xlim=c(0,50),ylim=c(-1,0),lwd=2)
lines(io_Explicit$Psi_LApo,col=2,lwd=2,lty=2)
lines(io_Implicit_SC$Psi_LApo,col=3,lwd=2)
lines(io_Explicit_SC$Psi_LApo,type='l',col=4,lwd=2,lty=2)

plot(io_Implicit$Psi_LApo,col=1,type='l',xlim=c(450,500),ylim=c(-5,-4),lwd=2)
lines(io_Explicit$Psi_LApo,type='l',lty=2,lwd=2,col=2)
lines(io_Implicit_SC$Psi_LApo,col=3,lwd=2)
lines(io_Explicit_SC$Psi_LApo,type='l',col=4,lwd=2,lty=2)







