# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
# Test Launcher to run SurEau-ECOS (V4.0) on Champenoux
# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory



# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

climateData_path          <- paste0(mainDir,'/Compar_SurEauC/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 

soilParameters_path       <- paste0(mainDir,'/Compar_SurEauC/Soil_test_champenoux.csv')
vegetationParameters_path <- paste0(mainDir,'/Compar_SurEauC/VegetationParams_ComparSurEauC.csv')
output_path               <- paste0(mainDir,'/Compar_SurEauC/test.csv')        


# create model input files --------------------------------------------------
modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                numericalScheme = 'Implicit',
                                                defoliation = F,
                                                resetSWC=T)       

simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters  <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.373333 ,0.746666,1.119)) 
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)


# run SurEau-Ecos ---------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
        simulation_parameters = simulation_parameters, 
       climate_data = climate_data,
       stand_parameters = stand_parameters, 
       soil_parameters = soil_parameters,
       vegetation_parameters = vegetation_parameters)


# analyse outputs ---------------------------------------------------------
# # for analyses / subdaily time scales 
   filename  = paste0(mainDir,"/Compar_SurEauC/test.csv")
   DATA = read.csv(filename,header=T, dec='.',sep="")
   DATA$DD= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")

   plot(DATA$DD,DATA$Psi_LSym,type='l',col='firebrick1',ylim=c(-6,0))
   lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
   lines(DATA$DD,DATA$Psi_AllSoil,type='l',col='blue')
   par(new=T)
   plot(DATA$DD,DATA$PLC_TL,type='l',col=1, ylab="", yaxt="n",xlab="")
   axis(4)
   
   plot(DATA$DD,DATA$regulFact,type='l',col=1, ylab="", yaxt="n",xlab="")
   plot(DATA$DD,DATA$gcanopy_lim,type='l',col=1, ylab="",xlab="")
   plot(DATA$DD,DATA$gs_lim,type='l',col=1, ylab="",xlab="")

     
   plot(DATA$DD,DATA$transpiration_mm ,type='l',col=1, ylab="",xlab="")
   lines(DATA$DD,DATA$EminT,type='l',col=2, ylab="",xlab="")
   
   
