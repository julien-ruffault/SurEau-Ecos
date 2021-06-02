# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Reference Launcher to run SurEau-ECOS on test dataset 

# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>
#           <Herve Cochard (herve.cochard@inrae.fr)>


# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                                   # do not modify 


# set paths
climateData_path          <- paste0(mainDir,'/reference_simulation/test_climat.csv') 
soilParameters_path       <- paste0(mainDir,'/reference_simulation/Soil_test.csv')
vegetationParameters_path <- paste0(mainDir,'/reference_simulation/Parameters_test_quercus_evergreen.csv')
output_path               <-  paste0(mainDir,'/reference_simulation/Reference_simulation_subdaily_out.csv')



# Create input files and run SurEau-Ecos
modeling_options      <- create.modeling.options()  
simulation_parameters <- create.simulation.parameters(startYearSimulation=1990,                       
                                                      endYearSimulation=1990,
                                                      mainDir=mainDir,
                                                      outputType='LFMC_subdaily',
                                                      overWrite=T,
                                                      outputPath=output_path)


climate_data          <- create.climate.data(filePath = climateData_path, 
                                        modeling_options = modeling_options,
                                        simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.2 ,0.4,0.6)) 
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



# exemple output  Analysis -----------------------------------------------------

# for analyses / subdaily time scales 
filename  = paste0(mainDir,"/reference_simulation/Reference_simulation_subdaily_out.csv")
DATA = read.csv(filename,header=T, dec='.',sep="")
DATA$Time= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")
# plot Psis
plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-4,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$Time,DATA$Psi_TSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$Time,DATA$Psi_TApo,type='l',col='firebrick4')
lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('bottomright',legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_Trunk_Symplasm','Psi_Trunk_Apoplasm','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)

# plot water fluxes 
plot(DATA$Time,DATA$AET.C,type='l',col='blue',xlab='Time',ylab='hourly evapotranspiration (mm)')


# plot cavitation 
plot(DATA$Time,DATA$PLC_TL,type='l', col='springgreen4',ylim=c(0,50),xlab='Time',ylab='PLC')
lines(DATA$Time,DATA$PLC_Root,type='l',col='brown')
legend('topleft',legend=c('PLC_leaf','PLC_root'),
       col=c('springgreen4','brown'),lty=1,lwd=2,cex=0.8)


# plot fuel moisture 
plot(DATA$Time,DATA$LFMCSymp,type='l', col='springgreen4',xlab='Time',ylab='Fuel moisture content (% dry weight)',ylim=c(40,80))
lines(DATA$Time,DATA$LFMCApo,type='l',col='brown3',lwd=2)
lines(DATA$Time,DATA$LFMC,col='grey30')
legend('bottomleft',legend=c('LFMC_Symplasm','LFMC_Apoplasm','LFMC'),
       col=c('springgreen4','brown3','grey30'),lty=1,lwd=2,cex=0.8)







