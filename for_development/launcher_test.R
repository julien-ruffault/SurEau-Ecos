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

# output
output_path               <- paste0(mainDir,'/for_development/test.csv')        

# create input lists to run SureauR --------------------------------------------
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                thresholdMortality = 99,
                                                numericalScheme = 'Explicit',
                                                compOptionsForEvapo = "Custom" ,
                                                customSmallTimeStepInSec = 0.0001,
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



# run SurEau-Ecos --------------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
        simulation_parameters = simulation_parameters, 
       climate_data = climate_data,
       stand_parameters = stand_parameters, 
       soil_parameters = soil_parameters,
       vegetation_parameters = vegetation_parameters)


# output  Analysis -------------------------------------------------------------

# for analyses / subdaily time scales 
filename  = paste0(mainDir,"/for_development/test.csv")
DATA = read.csv(filename,header=T, dec='.',sep="")
head(DATA)
DATA$DD= as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

# plot Psis
plot(DATA$DD,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-6,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$DD,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$DD,DATA$Psi_SSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
lines(DATA$DD,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('topright',legend=c('Psi_Lsym','Psi_Lapo','Psi_Tsym','Psi_Tapo','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)



plot(DATA$DD, DATA$transpiration_mm,type='l')
lines(DATA$DD, DATA$soilEvaporation_mm,type='l',col='blue')
lines(DATA$DD,DATA$Emin_mm,type='l',col='red')
lines(DATA$DD,DATA$Emin_S_mm,type='l',col='green')

plot(DATA$DD,DATA$transpiration_mm+DATA$soilEvaporation_mm+DATA$Emin_mm+DATA$Emin_S_mm,type='l')



#axis.POSIXct(side=1,x=DATA$DD)
#axis(side=2,las=2)



# # for analyses / daily time scales 
# filename  = paste0(mainDir,"/Results_model/test.csv")
# DATA = read.csv(filename,header=T, dec='.',sep="")
# plot(DATA$daily_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# lines(DATA$daily_Psi_LApoMin,type='l',col='firebrick4')
# plot(DATA$daily_Psi_SSymMin)
# plot(DATA$daily_Psi_SApoMin)
# plot(DATA$daily_evaporation_mm)
# plot(DATA$daily_transpiration_mm)
# plot(DATA$daily_PLC_Stem_max)
# plot(DATA$daily_PLC_Leaf_max)


# # for analyses / yearly time scales 
# filename  = paste0(mainDir,"/Results_model/test.csv")
# DATA = read.csv(filename,header=T, dec='.',sep="")
#  print(DATA)
# plot(DATA$yearly_Psi_LSymMin,type='l',col='firebrick1',ylim=c(-6,0))
# lines(DATA$yearly_Psi_LApoMin,type='l',col='firebrick4')
# plot(DATA$yearly_evaporation_mm)
# plot(DATA$yearly_transpiration_mm)


