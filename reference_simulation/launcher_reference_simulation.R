# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Reference Launcher to run SurEau-ECOS on Champenoux and compute a reference output / 
# reference.output.Champenoux.csv
# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SureauR.functions.R'))                              # do not modify 

climateData_path          <- paste0(mainDir,'/reference_simulation/Climat_constant_test_champenoux_REF.csv') # <-- indicate here the path to input climate data 
soilParameters_path       <- paste0(mainDir,'/reference_simulation/Soil_test_champenoux_REF.csv')
vegetationParameters_path <- paste0(mainDir,'/reference_simulation/Parameters_test_quercus_champenoux_evergreen_REF.csv')

output_path<-  paste0(mainDir,'/reference_simulation/simulation_reference_23_04.csv')

modeling_options  <- create.modeling.options(constantClimate=T,
                                             stomatalRegFormulation="Sigmoid")                      # <-- indicate  modeling options 
simulation_parameters <- create.simulation.parameters(startYearSimulation=1990,                         # <-- indicate here simulation parameters
                                                      endYearSimulation=1990,
                                                      mainDir=mainDir,
                                                      outputType='diagnostic_subdaily',
                                                      overWrite=F,
                                                      outputPath=output_path)

### Create input files and run SurEau-Ecos--------------------------------------
climate_data <- create.climate.data(filePath=climateData_path, modeling_options=modeling_options, simulation_parameters=simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax=6, lat = 48.73, lon=6.23)
soil_parameters <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.373333 ,0.746666,1.119)) 
vegetation_parameters <- create.vegetation.parameters(filePath =vegetationParameters_path, stand_parameters = stand_parameters, modeling_options = modeling_options)

run.SurEauR(modeling_options = modeling_options ,
            simulation_parameters = simulation_parameters, 
            climate_data =climate_data,
            stand_parameters =stand_parameters, 
            soil_parameters = soil_parameters,
            vegetation_parameters=vegetation_parameters)
