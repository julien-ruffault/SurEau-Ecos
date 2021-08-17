# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Example launcher to run SurEau-Ecos

# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##


# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                                   # do not modify 

# set paths
climateData_path          <- paste0(mainDir,'/quick_start/climat_example.csv')
soilParameters_path       <- paste0(mainDir,'/quick_start/soil_example.csv')
vegetationParameters_path <- paste0(mainDir,'/quick_start/vegetation_example.csv')
output_path               <-  paste0(mainDir,'/quick_start/example_output_subdaily.csv')

# Create input files and run SurEau-Ecos
modeling_options      <- create.modeling.options()  
simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                       
                                                      endYearSimulation = 1990,
                                                      mainDir = mainDir,
                                                      outputType = 'LFMC_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)


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

# Example output loading an plotting  ------------------------------------------
filename  = paste0(mainDir,"/quick_start/example_output_subdaily.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

# plot Psis
plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-4,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$Time,DATA$Psi_TSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$Time,DATA$Psi_TApo,type='l',col='firebrick4')
lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('bottomright',legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_Trunk_Symplasm','Psi_Trunk_Apoplasm','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)



# plot meteorological conditions 
plot(DATA$Time,DATA$Tair,type='l',col='firebrick4',ylab='Air temperature (degC)', xlab='Time')
par(new=T)
barplot(DATA$PPT,col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,60))
axis(4,col='blue',col.ticks='blue')


# plot water fluxes 
plot(DATA$Time,DATA$transpiration_mm,type='l',col='blue',xlab='Time',ylab='water fluxes (mm/timestep)')
lines(DATA$Time,DATA$Emin_mm,col='forestgreen')
lines(DATA$Time,DATA$EminT_mm,col='brown4')
lines(DATA$Time,DATA$SoilEvaporation_mm,type='l',col='grey30')
legend('topright',legend=c('Transpiration','Emin','EminT','Soil'),
       col=c('blue','forestgreen','brown4','grey30'),lty=1,lwd=2,cex=0.8)

# plot cavitation 
plot(DATA$Time,DATA$PLC_Leaf,type='l', col='springgreen4',ylim=c(0,70),xlab='Time',ylab='PLC')
lines(DATA$Time,DATA$PLC_Trunk,type='l',col='brown')
legend('topleft',legend=c('PLC_Leaf','PLC_Trunk'),
       col=c('springgreen4','brown'),lty=1,lwd=2,cex=0.8)

# plot fuel moisture 
plot(DATA$Time,DATA$LFMCSymp,type='l', col='springgreen4',xlab='Time',ylab='Fuel moisture content (% dry weight)',ylim=c(20,80))
lines(DATA$Time,DATA$LFMCApo,type='l',col='brown3',lwd=2)
lines(DATA$Time,DATA$LFMC,col='grey30')
legend('bottomleft',legend=c('LFMC_Symplasm','LFMC_Apoplasm','LFMC'),
       col=c('springgreen4','brown3','grey30'),lty=1,lwd=2,cex=0.8)







