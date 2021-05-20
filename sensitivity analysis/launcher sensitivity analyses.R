# run variance-based sensitivity analysis on SurEau-Ecos. 
# script by julien Ruffault (julien.ruff@gmail.com) / 
# created : 17/05/2021 

#first tests to run SUREAU while moving two parameters only 
# Sensitivity tests were performed on the  case test in CHampenout, constant climate, evergreen vegetation


rm(list = ls()) # ClWBveg$params$ear environment
gc()            # Clear memory

library(sensitivity)
library(tictoc)

mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SureauR.functions.R'))                             # do not modify 


# following paths and files won't change throughout the sensitivity analysis
climateData_path  <- paste0(mainDir,'/datasets/test_data/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
modeling_options     <- create.modeling.options(timeStepForEvapo=2,
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                defoliation = F,
                                                resetSWC=T,
                                                compOptionsForEvapo="Fast")   
soilFile <- read.soil.file(filePath=paste0(mainDir,'/datasets/test_data/Soil_test_champenoux.csv'))
vegFile  <- read.vegetation.file(filePath=  paste0(mainDir,'/datasets/test_data/Parameters_test_quercus_champenoux_evergreen.csv'),modeling_options=modeling_options)

n=2000




# define parameters and values  -------------------------------------------
io  = data.frame(P50_VC_TL= runif(n,min=-4.5,max=-2), LAImax=runif(n,min=4.5,max=7.5))
X1 <- io[1:(n/2),]
X2 <- io[((n/2)+1):(n),]
x.model <- sobol2002(model = NULL, X1, X2, nboot = 1000)
dim(x.model$X)

saveRDS(x.model$X, file = paste0(mainDir,'/Results_model/params_SA.rds'))
PARAMS=x.model$X




# run model  --------------------------------------------------------------
tic()
for (i in 1:nrow(PARAMS)){

  output_path = paste0(mainDir,'/Results_model/SA_test_',i,'.csv')
    
  simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                        endYearSimulation = 1990,
                                                        mainDir= mainDir,
                                                        resolutionOutput = "yearly",
                                                        outputType = 'simple_yearly',
                                                        overWrite = T,
                                                        outputPath = output_path)
  climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
  stand_parameters <- create.stand.parameters(LAImax=PARAMS$LAImax[i],lat = 48.73, lon = 6.23)
  
  vegFile$P50_VC_TL=PARAMS$P50_VC_TL[i]
  
  soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(0.373333 ,0.746666,1.119)) 
  vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)
  

  run.SurEauR(modeling_options = modeling_options ,
              simulation_parameters = simulation_parameters, 
              climate_data = climate_data,
              stand_parameters = stand_parameters, 
              soil_parameters = soil_parameters,
              vegetation_parameters = vegetation_parameters)
  
}
toc()

# try to run model with parralelization -----------------------------------
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload the computer
registerDoParallel(cl)
tic()
foreach(i=1:nrow(PARAMS),.packages=c('lubridate','insol')) %dopar% {output_path = paste0(mainDir,'/Results_model/SA_test_',i,'.csv')

simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "yearly",
                                                      outputType = 'simple_yearly',
                                                      overWrite = T,
                                                      outputPath = output_path)
climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax=PARAMS$LAImax[i],lat = 48.73, lon = 6.23)

vegFile$P50_VC_TL=PARAMS$P50_VC_TL[i]

soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(0.373333 ,0.746666,1.119)) 
vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)


run.SurEauR(modeling_options = modeling_options ,
            simulation_parameters = simulation_parameters, 
            climate_data = climate_data,
            stand_parameters = stand_parameters, 
            soil_parameters = soil_parameters,
            vegetation_parameters = vegetation_parameters)

}
toc()


#X = PARAMS 
#PARAMS =read.csv(file = paste0(mainDir,'/Results_model/params_SA.csv'),header=T)




# merge results -----------------------------------------------------------
Y1=NULL
Y2=NULL

for (i in 1:nrow(PARAMS))
{
 io =read.csv(paste0(mainDir,'/Results_model/SA_test_',i,'.csv'),header=T, dec='.',sep="")
 Y1[i]  = io$yearly_dayOfDeath     
 Y2[i]    =  io$yearly_transpiration_mm  
}



plot(PARAMS$LAImax,Y1)
plot(PARAMS$P50_VC_TL,Y1)


tell(x.model,Y1)
print(x.model)
plot(x.model,ylim=c(0,1))


# autre method  / essai avec sensobol package-----------------------------------





library(sensobol)
N <- 1000
k <- 2 
params <- colnames(PARAMS)
R <- 10^3
type <- "norm"
conf <- 0.95


AAA = sobol_matrices(N = N, params = params)

plot_scatter(data = PARAMS, N = N, Y = Y1, params = params)

ind <- sobol_indices(Y = Y1, N = N, params = params, boot = TRUE, R = R, type = type, conf = conf)
ind.dummy <- sobol_dummy(Y = Y1, N = N, params = params, boot = TRUE, R = R)

plot(ind, dummy = ind.dummy)








