# created on 05/08/2021  
# @author : Julien Ruffault (julien.ruff@gmail.com) 
# compute sobol indices with the 'sensobol' package 
# uses parameters from the CompaSurEauC simulation 
# each parameters can vvary from -80 to 80 % of its initial value 


rm(list = ls()) # ClWBveg$params$ear environment
gc()            # Clear memory

library(sensobol)
library(foreach)
library(doParallel)
library(lubridate)


mainDir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))                 
mainDir
source(paste0(mainDir,'/functions/load.SurEau-Ecos.R'))                   
directoryToRefSimu = paste0(mainDir,'/projects/Compar_SurEauC')

# keeping temporary simulation output files in a local directory
PC=F
if (PC){}
if(!PC){dir_SA_files = "/Users/jruffault/Dropbox/taf/temporary_SA/"}


# following paths and files won't change throughout the sensitivity analysis
climateData_path  <- paste0(directoryToRefSimu,'/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
modeling_options  <- create.modeling.options(timeStepForEvapo=2,
                                             constantClimate=T,
                                             defoliation = F,
                                             compOptionsForEvapo = c("Fast"),
                                             thresholdMortality=88)
soilFile <- read.soil.file(filePath=paste0(directoryToRefSimu,'/Soil_test_champenoux.csv'))
vegFile  <- read.vegetation.file(filePath=  paste0(directoryToRefSimu,'/VegetationParams_ComparSurEauC.csv'),modeling_options=modeling_options)



params=c('P50_VC_Leaf','P50_gs','kPlantInit','gsMax','gmin20','VolumeLiving_TRB','LAImax','SWC')
#
N <- 50 # number for initial sampling / about 30000 for full models
k <- length(params) # number of parameters

R <- 10^3
type <- "norm"
conf <- 0.95
PARAMS= sobol_matrices(params=params,N=N,order="first")

print(paste0("number of simulations : ",nrow(PARAMS)))


PARAMS[, "LAImax"] <- qunif(PARAMS[, "LAImax"], 6-0.5*6, 6+0.5*6)
PARAMS[,"SWC"]  <-  qunif(PARAMS[, "SWC"], 201-201*0.5,201+201*0.5)
PARAMS[, "P50_VC_Leaf"] <- qunif(PARAMS[, "P50_VC_Leaf"], vegFile$P50_VC_Leaf+vegFile$P50_VC_Leaf*0.5, vegFile$P50_VC_Leaf-vegFile$P50_VC_Leaf*0.5)
PARAMS[, "gmin20"] <- qunif(PARAMS[, "gmin20"], vegFile$gmin20-0.5*vegFile$gmin20,vegFile$gmin20+0.5*vegFile$gmin20)
PARAMS[,"kPlantInit"] <-  qunif(PARAMS[, "kPlantInit"], vegFile$kPlantInit-0.5*vegFile$kPlantInit,vegFile$kPlantInit+0.5*vegFile$kPlantInit)
PARAMS[, "VolumeLiving_TRB"] <- qunif(PARAMS[, "VolumeLiving_TRB"], vegFile$VolumeLiving_TRB-0.5*vegFile$VolumeLiving_TRB,vegFile$VolumeLiving_TRB+0.5*vegFile$VolumeLiving_TRB)
PARAMS[, "gsMax"] <- qunif(PARAMS[, "gsMax"], vegFile$gsMax-0.5*vegFile$gsMax,vegFile$gsMax+0.5*vegFile$gsMax)
PARAMS[, "P50_gs"] <- qunif(PARAMS[, "P50_gs"],vegFile$P50_gs+vegFile$P50_gs*0.5, vegFile$P50_gs-vegFile$P50_gs*0.5)


head(PARAMS)

DEPTH =PARAMS[,"SWC"] / ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)
# # to input a desired value
# var = readline(prompt  = "run SA simulation  ?  this will overwrite the precedent simulation (T or F)   :   ")

# # convert the inputted value to integer
# var = as.logical(var);
# 
# 
# if (var!=T){stop('stop running')}
# # print the value



write.csv(PARAMS,paste0(dir_SA_files,'/PARAMS_.csv'),row.names=F)

vegFile$gCrown0=10000

# run the model 
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload the computer
registerDoParallel(cl)
#tic()
foreach(i=1:nrow(PARAMS),.packages=c('lubridate','insol')) %dopar% {
  print(i)
  output_path = paste0(dir_SA_files,'/SA_test_',i,'.csv')
  
  simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                        endYearSimulation = 1990,
                                                        mainDir= mainDir,
                                                        resolutionOutput = "yearly",
                                                        outputType = 'simple_yearly',
                                                        overWrite = T,
                                                        outputPath = output_path)
  climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
  climate_data$WS_mean <- 10000
  stand_parameters <- create.stand.parameters(LAImax=PARAMS[,"LAImax"][i],lat = 48.73, lon = 6.23)
  #stand_parameters <- create.stand.parameters(LAImax=6,lat = 48.73, lon = 6.23)
  
  
  
  vegFile$P50_VC_Leaf=PARAMS[,"P50_VC_Leaf"][i]
  vegFile$P50_VC_Trunk = PARAMS[,"P50_VC_Leaf"][i]
  # 
  
  vegFile$gmin20=PARAMS[,"gmin20"][i]
  vegFile$gmin_T=PARAMS[,"gmin20"][i]
  vegFile$kPlantInit=PARAMS[,"kPlantInit"][i]
  vegFile$VolumeLiving_TRB=PARAMS[,"VolumeLiving_TRB"][i]
  vegFile$gsMax = PARAMS[,"gsMax"][i]
  
  vegFile$P50_gs = PARAMS[,"P50_gs"][i]
  vegFile$P88_gs = vegFile$P50_gs-0.55/2
  vegFile$P12_gs = vegFile$P50_gs+0.55/2
  vegFile$slope_gs = 100/(vegFile$P12_gs-vegFile$P88_gs)
  
  
  
  #soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(0.573333 ,0.746666,1.119)) 
  
  soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, depths = c(DEPTH[i]*1/3 ,DEPTH[i]*2/3,DEPTH[i]))
  
  vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)
  
  
  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)
}

PARAMS = read.csv(paste0(dir_SA_files,'/PARAMS_.csv')) # PARAMS file should be saved too in the final versions to keep memory of the parameters used ! 

params=colnames(PARAMS)
Y1=NULL
Y2=NULL
for (i in 1:nrow(PARAMS))
{
  io =read.csv(paste0(dir_SA_files,'/SA_test_',i,'.csv'),header=T, dec='.',sep="")
  Y1[i]  = io$yearly_dayOfDeath
}
R=10e3
type <- "norm"
conf <- 0.95
plot_scatter(data=PARAMS,N=N,Y=Y1,params=params)
#plot_scatter(data=PARAMS,N=N,Y=Y2,params=params)



plot_multiscatter(data = PARAMS, N = N, Y = Y1, params = params)
ind <- sobol_indices(Y = Y1, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
plot(ind)
ind.dummy <- sobol_dummy(Y = Y1, N = N, params = params, boot = TRUE, R = R)
plot(ind,dummy=ind.dummy)

