# creation :  12/11/2020 add other traits for SuReaU compared to previsou scripts 
# Author : Julien Ruffault (julien.ruff@gmail.com)
# Perform sensitivity tests on SurEau-Ecos using for multiple vegetation types for a limited number of selected traits 

#
# test to include a few other traits : 
#  - gcanopy --> 
#  - betaRootProfile --> OK
#  - Kleafsymp   --> OK
#  - Ktsymp      --> OK
#  - pi0 leaf 
#  - espilon 
#  - slope _VC    --> OK 
#  - slope gs   --> OK 


# each parameters can vary from +/- X % of its initial value 


#  [8] "vegetation_spatial_Fagus.csv"                             
#  [9] "vegetation_spatial_QI.csv"                                
#  [10] "VegetationParams_ComparSurEauC.csv"     



# clean environment
rm(list=ls(all=TRUE));gc()
# define working directory as the directory of Sureau
#mainDir <- dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
mainDir = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/SurEau-Ecos"

#define directory where inputs parameters are stocked 
#directoryToRefSimu = dirname(rstudioapi::getSourceEditorContext()$path)
directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/SurEau-Ecos/projects/sensitivity_analysis2"




mainDirOutpout = paste0(directoryToRefSimu,'/Results_new_simu_2/')

library(sensobol)
library(foreach)
library(doParallel)
library(lubridate)

source(paste0(mainDir,'/functions/load.SurEau-Ecos.R'))   # load SuReau-Ecos                 

# climate and soils won't change throughout the sensitivity analysis and are loaded here 
climateData_path  <- paste0(directoryToRefSimu,'/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
modeling_options  <- create.modeling.options(timeStepForEvapo=1,
                                             constantClimate=T,
                                             defoliation = F,
                                             compOptionsForEvapo = c("Fast"),
                                             thresholdMortality=99)
soilFile <- read.soil.file(filePath=paste0(directoryToRefSimu,'/Soil_test_champenoux.csv'))




# Define general parameters for the simulation
params=c('LAImax','SWC',
         'P50_VC_Leaf','slope_VC_Leaf',
         'gmin20','g_canopy',
         'P50_gs','slope_gs',
         'kPlantInit','VolumeLiving_TRB','betaRootProfile',
         "k_TSymInit","fracLeafSym",
         "EpsilonSymp",
         "PiFullTurgor",
         "Q10_1_gmin",
         "Q10_2_gmin")


# 
# params=c('LAImax','SWC',
#          'gmin20','g_canopy',
#          'P50_gs')




percentV = 20/100
N <- 3 # number for initial sampling / about 30000-100000 for full models
k <- length(params) # number of parameters

R <- 10^3
type <- "norm"
conf <- 0.95


# for parallelistation 
cores=detectCores()
cl <- makeCluster(cores[1]-4) #not to overload the computer
registerDoParallel(cl)


files <- c("/vegetation_spatial_Fagus.csv", "vegetation_spatial_QI.csv", "VegetationParams_ComparSurEauC.csv")
LAI <- c(5, 3, 6) #
path_outputs <- c("Fagus", "QI", "Petraea")
SWCmean <- 160




# for (v in 1:3){ # loop on vegetation types
v <- 1
vegFile <- read.vegetation.file(filePath = file.path(directoryToRefSimu, files[v]), modeling_options = modeling_options)

#vegFile$gCrown0 <- 10000 # to avoid the efffect of gcrown

PARAMS <- sobol_matrices(params = params, N = N, order = "first")
print(paste0('number of simulations : ',nrow(PARAMS)))


PARAMS[, "LAImax"] <- qunif(PARAMS[, "LAImax"], LAI[v] - percentV * LAI[v], LAI[v] + percentV * LAI[v])
PARAMS[, "SWC"] <- qunif(PARAMS[, "SWC"], SWCmean - SWCmean * percentV, SWCmean + SWCmean * percentV)
PARAMS[, "P50_VC_Leaf"] <- qunif(PARAMS[, "P50_VC_Leaf"], vegFile$P50_VC_Leaf + vegFile$P50_VC_Leaf * percentV, vegFile$P50_VC_Leaf - vegFile$P50_VC_Leaf * percentV)

PARAMS[, "gmin20"] <- qunif(PARAMS[, "gmin20"], vegFile$gmin20 - percentV * vegFile$gmin20, vegFile$gmin20 + percentV * vegFile$gmin20)
PARAMS[, "g_canopy"] <- qunif(PARAMS[, "g_canopy"], 45- percentV * 45, 45 + percentV * 45)

PARAMS[, "kPlantInit"] <- qunif(PARAMS[, "kPlantInit"], vegFile$kPlantInit - percentV * vegFile$kPlantInit, vegFile$kPlantInit + percentV * vegFile$kPlantInit)

PARAMS[, "k_TSymInit"] <- qunif(PARAMS[, "k_TSymInit"], 0.26 - percentV * 0.26, 0.26 + percentV * 0.26)
PARAMS[, "fracLeafSym"] <- qunif(PARAMS[, "fracLeafSym"], vegFile$fracLeafSym- percentV * vegFile$fracLeafSym, vegFile$fracLeafSym + percentV * vegFile$fracLeafSym)

PARAMS[, "slope_gs"] <- qunif(PARAMS[, "slope_gs"], 200 - percentV * 200, 200 + percentV * 200)

PARAMS[, "VolumeLiving_TRB"] <- qunif(PARAMS[, "VolumeLiving_TRB"], vegFile$VolumeLiving_TRB - percentV * vegFile$VolumeLiving_TRB, vegFile$VolumeLiving_TRB + percentV * vegFile$VolumeLiving_TRB)

PARAMS[, "slope_VC_Leaf"] <- qunif(PARAMS[, "slope_VC_Leaf"], vegFile$slope_VC_Leaf - percentV * vegFile$slope_VC_Leaf, vegFile$slope_VC_Leaf + percentV * vegFile$slope_VC_Leaf)

PARAMS[, "P50_gs"] <- qunif(PARAMS[, "P50_gs"], vegFile$P50_gs + vegFile$P50_gs * percentV, vegFile$P50_gs - vegFile$P50_gs * percentV)
PARAMS[, "betaRootProfile"] <- qunif(PARAMS[, "betaRootProfile"], vegFile$betaRootProfile - 0.04 * percentV, vegFile$betaRootProfile + 0.04 * percentV)

 PARAMS[, "EpsilonSymp"] <- qunif(PARAMS[, "EpsilonSymp"], vegFile$EpsilonSymp_Leaf - percentV * vegFile$EpsilonSymp_Leaf, vegFile$EpsilonSymp_Leaf + percentV * vegFile$EpsilonSymp_Leaf)
 PARAMS[, "PiFullTurgor"] <- qunif(PARAMS[, "PiFullTurgor"], vegFile$PiFullTurgor_Leaf + percentV * vegFile$PiFullTurgor_Leaf, vegFile$PiFullTurgor_Leaf - percentV * vegFile$PiFullTurgor_Leaf)
# 
 PARAMS[, "Q10_1_gmin"] <- qunif(PARAMS[, "Q10_1_gmin"], vegFile$Q10_1_gmin - percentV * vegFile$Q10_1_gmin, vegFile$Q10_1_gmin + percentV * vegFile$Q10_1_gmin)
 PARAMS[, "Q10_2_gmin"] <- qunif(PARAMS[, "Q10_2_gmin"], vegFile$Q10_2_gmin - percentV * vegFile$Q10_2_gmin, vegFile$Q10_2_gmin + percentV * vegFile$Q10_2_gmin)
#   
#   
  
  
  
  
DEPTH =PARAMS[,"SWC"] / ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)
  
  
  
  
  
  
  
  
  # write output directory 
  Out_dir <- file.path(mainDirOutpout,path_outputs[v])
  dir.create(Out_dir,showWarnings=F,recursive=T)
  write.csv(PARAMS,paste0(Out_dir,'/PARAMS_.csv'),row.names=F)
  
  
  foreach(i=1:nrow(PARAMS),.packages=c('lubridate','insol')) %dopar% {
    print(i)
    output_path = paste0(Out_dir,'/SA_test_',i,'.csv')
    
    
    simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                          endYearSimulation = 1990,
                                                          mainDir= mainDir,
                                                          resolutionOutput = "yearly",
                                                          outputType = 'yearly_forSA',
                                                          overWrite = T,
                                                          outputPath = output_path)
    climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
    
    
    climate_data$WS_mean <- 1
    
#    gCrown0*windSpeed^0.6
    
    
    stand_parameters <- create.stand.parameters(LAImax=PARAMS[,"LAImax"][i],lat = 48.73, lon = 6.23)
    #stand_parameters <- create.stand.parameters(LAImax=6,lat = 48.73, lon = 6.23)
    

    vegFile$gCrown0=PARAMS[,"g_canopy"][i]
    
    vegFile$gmin20=PARAMS[,"gmin20"][i]
    vegFile$gmin_T=PARAMS[,"gmin20"][i]

    vegFile$P50_gs = PARAMS[,"P50_gs"][i]
    
    
    vegFile$P50_VC_Leaf=PARAMS[,"P50_VC_Leaf"][i]
    vegFile$P50_VC_Trunk = PARAMS[,"P50_VC_Leaf"][i]
    vegFile$kPlantInit=PARAMS[,"kPlantInit"][i]
    vegFile$slope_VC_Leaf=PARAMS[,"slope_VC_Leaf"][i]
    vegFile$slope_VC_Trunk= PARAMS[,"slope_VC_Leaf"][i]
    vegFile$slope_gs  = PARAMS[,"slope_gs"][i]
    vegFile$betaRootProfile = PARAMS[,"betaRootProfile"][i]
    vegFile$EpsilonSymp_Leaf = PARAMS[,"EpsilonSymp"][i]
    vegFile$EpsilonSymp_Trunk = PARAMS[,"EpsilonSymp"][i]
    vegFile$PiFullTurgor_Leaf = PARAMS[,"PiFullTurgor"][i]
    vegFile$PiFullTurgor_Trunk = PARAMS[,"PiFullTurgor"][i]
    vegFile$Q10_1_gmin = PARAMS[,"Q10_1_gmin"][i]
    vegFile$Q10_2_gmin = PARAMS[,"Q10_2_gmin"][i]
    vegFile$VolumeLiving_TRB=PARAMS[,"VolumeLiving_TRB"][i]

    vegFile$fracLeafSym = PARAMS[,"fracLeafSym"][i]
    
    
    soilFile$depth1 = DEPTH[i]*1/3
    soilFile$depth2 = DEPTH[i]*2/3
    soilFile$depth3 = DEPTH[i]
    
    soil_parameters  <- create.soil.parameters(listOfParameters = soilFile)
    vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)
    
    vegetation_parameters$k_TSymInit = PARAMS[,"k_TSymInit"][i]
    #vegetation_parameters$k_LSymInit = PARAMS[,"k_LSymInit"][i]
    
    
    run.SurEau_Ecos(modeling_options = modeling_options ,
                    simulation_parameters = simulation_parameters, 
                    climate_data = climate_data,
                    stand_parameters = stand_parameters, 
                    soil_parameters = soil_parameters,
                    vegetation_parameters = vegetation_parameters)
  }
  
  
  
  
#}

 v=1
    Out_dir <- file.path(mainDirOutpout,path_outputs[v])
    
    PARAMS =   io =read.csv(paste0(Out_dir,'/PARAMS_.csv'),header=T, dec='.',sep=",")
    
    params=colnames(PARAMS)
    Y1 = NULL
    Y2 = NULL
    Y3 = NULL
    
    for (i in 1:nrow(PARAMS))
    {
      io =read.csv(paste0(Out_dir,'/SA_test_',i,'.csv'),header=T, dec='.',sep="")
      Y1[i]  = io$yearly_dayOfDeath
      Y2[i]  = io$yearly_dayOfStomatalClosure 
      Y3[i]  = io$yearly_dayOfDeath - io$yearly_dayOfStomatalClosure 
    }
    
  plot_scatter(data=PARAMS,N=N,Y=Y1,params=params)
  plot_scatter(data=PARAMS,N=N,Y=Y2,params=params)
  plot_scatter(data=PARAMS,N=N,Y=Y3,params=params)
  
  IND1 = sobol_indices(Y = Y1, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  IND2 = sobol_indices(Y = Y2, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  IND3 = sobol_indices(Y = Y3, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  
  plot(IND1)
  plot(IND2)
  plot(IND3)
  
  
  IND1 = sobol_indices(Y = Y1, N = N, params = params, order="first",boot = F)
  IND2 = sobol_indices(Y = Y2, N = N, params = params, order="first",boot = F)
  IND3 = sobol_indices(Y = Y3, N = N, params = params, order="first",boot = F)
  
  #quartz(width=8,height=8)
  plot(IND1)
  
  plot(IND2)
  plot(IND3)
  
  
  
  
  #plot_multiscatter(data = PARAMS, N = N, Y = Y1, params = params)
  # IND1[[v]] <- sobol_indices(Y = Y1, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  # plot(IND1[[v]])
  # 
  # 
  # IND2[[v]] <- sobol_indices(Y = Y2, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  # plot(IND2[[v]])
  # 
  # IND3[[v]] <- sobol_indices(Y = Y3, N = N, params = params, order="first",boot = TRUE, R = R, type = type, conf = conf)
  # plot(IND3[[v]])
  # 
  
  