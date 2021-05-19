
# Initialization ---------------------------------------------------------------
rm(list = ls()) # ClWBveg$params$ear environment
gc()            # Clear memory


# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SureauR.functions.R'))                             # do not modify 

climateData_path          <- paste0(mainDir,'/myriam_data/climat_SAFRAN_myriam_puechabon.csv') # <-- indicate here the path to input climate data 

soilParameters_path       <- paste0(mainDir,'/myriam_data/Soil_Puechabon_myriam.csv')
vegetationParameters_path <- paste0(mainDir,'/myriam_data/Veg_Puech_myriam.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  
output_path               <- paste0(mainDir,'/myriam_data/test.csv')        


modeling_options     <- create.modeling.options(defoliation=T)


simulation_parameters <- create.simulation.parameters(startYearSimulation = 2003,                        
                                                      endYearSimulation = 2003,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

### Create input files and run SurEau-Ecos--------------------------------------
climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax = 2.1, lat = 43.5, lon = 3.4)

soil_parameters  <- create.soil.parameters(filePath=soilParameters_path) 
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)


run.SurEauR(modeling_options = modeling_options ,
            simulation_parameters = simulation_parameters, 
            climate_data = climate_data,
            stand_parameters = stand_parameters, 
            soil_parameters = soil_parameters,
            vegetation_parameters = vegetation_parameters)


# for output analysis 


# for analyses / subdaily time scales
  filename  = paste0(mainDir,"/myriam_data/test.csv")
  DATA = read.csv(filename,header=T, dec='.',sep="")
  DATA$DD= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")


  plot(DATA$DD,DATA$Psi_LSym,type='l',col='firebrick1',ylim=c(-6,0))
  lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')

  lines(DATA$DD,DATA$Psi_TSym,type='l',col='blue',ylim=c(-8,0))
  lines(DATA$DD,DATA$Psi_TApo,type='l',col='lightblue')

  lines(DATA$DD,DATA$Psi_AllSoil,type='l',lwd=2)

  plot(DATA$DD,DATA$LAI)
  plot(DATA$DD,DATA$LAIdead)

  
  plot(DATA$PLC_TL)
  lines(DATA$PLC_Root,col='red')
  

  # plot des LFMC pour tests
   plot(DATA$DD,DATA$LFMC,type='l')
   plot(DATA$DD,DATA$LFMCApo)
   plot(DATA$DD,DATA$LFMCSymp,type='l')
   plot(DATA$DD,DATA$FMCCanopy,type='l')
   plot(DATA$DD,DATA$DFMC,type='l')

  # plot des conductances
  plot(DATA$DD,DATA$k_Root1,type='l',ylim=c(0,4),lwd=1.5)
  lines(DATA$DD,DATA$k_Root2,col='green',lwd=1.5)
  lines(DATA$DD,DATA$k_Root3,col='red',lwd=1.5)
  lines(DATA$DD,DATA$k_TL,col='blue',lwd=1.5)
  lines(DATA$DD,DATA$k_LSym,col='pink',lwd=1.5)
  lines(DATA$DD,DATA$k_TSym,col='grey30',lwd=1.5)





