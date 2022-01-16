# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher to run SurEau-Ecos on an exemple case
# date : 14/01/2022
# authors : Julien Ruffault (julien.ruff@gmail.com)
#           Nicolas Martin  (nicolas.martin@inrae.fr)
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory
# Set paths  -----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
climateData_path          <- paste0(mainDir,'/quick_start/climat_example.csv')
soilParameters_path       <- paste0(mainDir,'/quick_start/soil_example.csv')
vegetationParameters_path <- paste0(mainDir,'/quick_start/vegetation_example.csv')
output_path               <-  paste0(mainDir,'/quick_start/example_output_subdaily.csv')
# Load model -------------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) # do not modify 
# Create input list files and define options -----------------------------------
modeling_options <- create.modeling.options()    
simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                       
                                                      endYearSimulation = 1990,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                        modeling_options = modeling_options,
                                        simulation_parameters = simulation_parameters) #
stand_parameters      <- create.stand.parameters(LAImax = 4.5, lat = 48.73, lon = 6.23)
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, 
                                                modeling_options = modeling_options) 
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

# Plot outputs -----------------------------------------------------------------
# load output file   
filename  = paste0(mainDir,"/quick_start/example_output_subdaily.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
# plot Plant water potentials
plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-4,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$Time,DATA$Psi_SSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$Time,DATA$Psi_SApo,type='l',col='firebrick4')
lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('bottomright',
       legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_Stem_Symplasm','Psi_Stem_Apoplasm','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)

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

# plot fuel moisture 
plot(DATA$Time,DATA$LFMCSymp,type='l', col='springgreen4',xlab='Time',ylab='Fuel moisture content (% dry weight)',ylim=c(20,80))
lines(DATA$Time,DATA$LFMCApo,type='l',col='brown3',lwd=2)
lines(DATA$Time,DATA$LFMC,col='grey30')
legend('bottomleft',legend=c('LFMC_Symplasm','LFMC_Apoplasm','FMC_Canopy'),
       col=c('springgreen4','brown3','grey30'),lty=1,lwd=2,cex=0.8)
