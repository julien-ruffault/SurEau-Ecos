# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
#  Launcher to run tests on  SurEau-ECOS-v1.0.x 
#  test for explicit numericalScheme / parametrisation based on the comparison with Sureau.C
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

# climate data 
climateData_path          <- paste0(mainDir,'/projects/Compar_SurEauC/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 


# Vegetation and soil parameters
soilParameters_path       <- paste0(mainDir,'/projects/Compar_SurEauC/Soil_test_champenoux.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/Compar_SurEauC/VegetationParams_ComparSurEauC.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  

for (SECONDS in c(5,10,15,20,30,35,40,45,50,100))

{

# output
output_path               <- paste0(mainDir,'/for_development/test_explicit_C100_',SECONDS,'s.csv')        

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
 vegetation_parameters$C_LApoInit  = 10
 vegetation_parameters$C_SApoInit  = 10


 # vegetation_parameters$C_LApoInit  = 1e-100
 # vegetation_parameters$C_SApoInit  = 1e-100

# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)
}

# version implicite pour comparaison

# output
output_path               <- paste0(mainDir,'/for_development/test_explicit_C100_implicit','.csv')        

#create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             stomatalRegFormulation = "Sigmoid",
                                             thresholdMortality = 99,
                                             numericalScheme = 'Implicit',
                                             defoliation = F,
                                             Lcav=0,
                                             Scav=0,
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
vegetation_parameters$C_LApoInit  = 10
vegetation_parameters$C_SApoInit  = 10


# vegetation_parameters$C_LApoInit  = 1e-100
# vegetation_parameters$C_SApoInit  = 1e-100

# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)












# output  Analysis -------------------------------------------------------------
DATA = NULL
count=0
for (SECONDS in c(5,10,15,20,30,50,100)){
  count=count+1
  # for analyses / subdaily time scales 
  filename  = paste0(mainDir,'/for_development/test_explicit_C100_',SECONDS,'s.csv')        
  io= read.csv(filename,header=T, dec='.',sep="")
  io$DD= as.POSIXct(io$Time,format='%Y-%m-%d/%H:%M:%S')

  DATA[[count]] = io
}

DATA_implicit =   filename  = paste0(mainDir,'/for_development/test_explicit_C100_implicit.csv')        
DATA_implicit= read.csv(filename,header=T, dec='.',sep="")
DATA_implicit$DD= as.POSIXct(DATA_implicit$Time,format='%Y-%m-%d/%H:%M:%S')

plot(DATA_implicit$Psi_LApo,type='l')
lines(DATA[[1]]$Psi_LApo,type='l',col=2)
lines(DATA[[2]]$Psi_LApo,col=3)
lines(DATA[[3]]$Psi_LApo,col=4)
lines(DATA[[4]]$Psi_LApo,col=5)
lines(DATA[[5]]$Psi_LApo,col=6)
lines(DATA[[6]]$Psi_LApo,col=8)
#lines(DATA[[7]]$Psi_LApo,col=1)

SECONDS = c(5,10,15,20,30,50)



legend('topright',col=c(1:6,8),      legend=  c('implicit',paste0(' explicit:',SECONDS,'s')),lty=1)

# plot(DATA$Psi_LApo,type='l')
# lines(DATA$Psi_SApo,type='l',col='red')

# # plot Psis
plot(DATA$DD,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-6,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$DD,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$DD,DATA$Psi_SSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
lines(DATA$DD,DATA$Psi_AllSoil,col='grey20',lwd=2)
# legend('topright',legend=c('Psi_Lsym','Psi_Lapo','Psi_Tsym','Psi_Tapo','Psi_Soil'),
#        col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)
# 
# 
# 
# plot(DATA$DD, DATA$transpiration_mm,type='l')
# lines(DATA$DD, DATA$soilEvaporation_mm,type='l',col='blue')
# lines(DATA$DD,DATA$Emin_mm,type='l',col='red')
# lines(DATA$DD,DATA$Emin_S_mm,type='l',col='green')
# 
# plot(DATA$DD,DATA$transpiration_mm+DATA$soilEvaporation_mm+DATA$Emin_mm+DATA$Emin_S_mm,type='l')
# 
# 
# 
# #axis.POSIXct(side=1,x=DATA$DD)
# #axis(side=2,las=2)
# 
# 
# 
# # # for analyses / daily time scales 
# # filename  = paste0(mainDir,"/Results_model/test.csv")
# # DATA = read.csv(filename,header=T, dec='.',sep="")
# # plot(DATA$daily_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# # lines(DATA$daily_Psi_LApoMin,type='l',col='firebrick4')
# # plot(DATA$daily_Psi_SSymMin)
# # plot(DATA$daily_Psi_SApoMin)
# # plot(DATA$daily_evaporation_mm)
# # plot(DATA$daily_transpiration_mm)
# # plot(DATA$daily_PLC_Stem_max)
# # plot(DATA$daily_PLC_Leaf_max)
# 
# 
# # # for analyses / yearly time scales 
# # filename  = paste0(mainDir,"/Results_model/test.csv")
# # DATA = read.csv(filename,header=T, dec='.',sep="")
# #  print(DATA)
# # plot(DATA$yearly_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# # lines(DATA$yearly_Psi_LApoMin,type='l',col='firebrick4')
# # plot(DATA$yearly_evaporation_mm)
# # plot(DATA$yearly_transpiration_mm)
# 
# 
