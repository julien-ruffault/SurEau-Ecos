# script cree le 17/05/2021 par JR 
# compute sobol indices with the 'sensobol' package 
# 


rm(list = ls()) # ClWBveg$params$ear environment
gc()            # Clear memory

PC=F
#PC=T

if (PC){
#dir_SA_files =   
}

if(!PC){dir_SA_files = "/Users/jruffault/Dropbox/taf/temporary_SA/"}



library(foreach)
library(doParallel)

mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SureauR.functions.R'))                             # do not modify 


# following paths and files won't change throughout the sensitivity analysis
climateData_path  <- paste0(mainDir,'/datasets/test_data/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
modeling_options     <- create.modeling.options(timeStepForEvapo=2,
                                                constantClimate=T,
                                                stomatalRegulationType = "Sigmoid",
                                                defoliation = F,
                                                resetSWC=T,
                                                compOptionsForEvapo="Fast")   
soilFile <- read.soil.file(filePath=paste0(mainDir,'/datasets/test_data/Soil_test_champenoux.csv'))
vegFile  <- read.vegetation.file(filePath=  paste0(mainDir,'/datasets/test_data/Parameters_test_quercus_champenoux_evergreen.csv'),modeling_options=modeling_options)




# parameters test that had no influence on time to death : gsMax, alpah_vg, Ksat 


library(sensobol)
N <- 20 # number for initial sampling 
k <- 2 # number of parameters
params <- c('LAImax','P50_VC_TL','gmin20','kPlantInit','SWC','gsMax')

params <- c('LAImax','betaRootProfile')

R <- 10^3
type <- "norm"
conf <- 0.95
PARAMS= sobol_matrices(params=params,N=N)
#save.csv(PARAMS,)
print(paste0("number of simulations : ",length(PARAMS)))



 PARAMS[, "LAImax"] <- qunif(PARAMS[, "LAImax"], 4, 8)
# PARAMS[, "P50_VC_TL"] <- qunif(PARAMS[, "P50_VC_TL"], -4.5, -2)
# PARAMS[, "gmin20"] <- qunif(PARAMS[, "gmin20"], 2.5,6)
# PARAMS[,"kPlantInit"] <-  qunif(PARAMS[, "kPlantInit"], 0.3,1)
# PARAMS[,"SWC"] <-  qunif(PARAMS[, "SWC"], 150,250)


PARAMS[,"betaRootProfile"] <-  qunif(PARAMS[, "betaRootProfile"], 0.95,0.99)






#DEPTH =PARAMS[,"SWC"] / ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)

# run the model 
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload the computer
registerDoParallel(cl)
#tic()
foreach(i=1:nrow(PARAMS),.packages=c('lubridate','insol')) %dopar% {
  
  output_path = paste0(dir_SA_files,'/SA_test_',i,'.csv')

  simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "yearly",
                                                      outputType = 'simple_yearly',
                                                      overWrite = T,
                                                      outputPath = output_path)
  climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
  stand_parameters <- create.stand.parameters(LAImax=PARAMS[,"LAImax"][i],lat = 48.73, lon = 6.23)

  
  vegFile$betaRootProfile  = PARAMS[,"betaRootProfile"][i]
  
  # vegFile$gsMax=PARAMS[,"gsMax"][i]
  # vegFile$P50_VC_TL=PARAMS[,"P50_VC_TL"][i]
  # vegFile$gmin20=PARAMS[,"gmin20"][i]
  # vegFile$kPlantInit=PARAMS[,"kPlantInit"][i]
  # 
  # 
  # soilFile$alpha_vg = PARAMS[,"alpha_vg"][i]

 

  
  
  
  
  soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(0.373333 ,0.746666,1.119)) 

  #soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(DEPTH[i]*1/3 ,DEPTH[i]*2/3,DEPTH[i]))

  vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)


  run.SurEauR(modeling_options = modeling_options ,
            simulation_parameters = simulation_parameters, 
            climate_data = climate_data,
            stand_parameters = stand_parameters, 
            soil_parameters = soil_parameters,
            vegetation_parameters = vegetation_parameters)

}


Y1=NULL
for (i in 1:nrow(PARAMS))
{
  io =read.csv(paste0(dir_SA_files,'/SA_test_',i,'.csv'),header=T, dec='.',sep="")
  Y1[i]  = io$yearly_dayOfDeath
}
#PARAMS = read.csv()

plot_scatter(data=PARAMS,N=N,Y=Y1,params=params)
ind <- sobol_indices(Y = Y1, N = N, params = params, boot = TRUE, R = R, type = type, conf = conf)
plot(ind)
ind.dummy <- sobol_dummy(Y = Y1, N = N, params = params, boot = TRUE, R = R)
plot(ind,dummy=ind.dummy)

