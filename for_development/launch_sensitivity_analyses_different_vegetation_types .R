# creation :  13/11/2021
# Author : Julien Ruffault (julien.ruff@gmail.com)
# Perform sensitivity tests on SurEau-Ecos using for multiple vegetation types for a limited number of selected traits 
# first-order and total-order indices are compuated (sensobol)
# V0:13/11/2021
# 
# each parameters can vary from +/- X % of its initial value 

# clean environment
rm(list=ls(all=TRUE));gc()
# define working directory as the directory of Sureau
mainDir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
mainDir # to check validity of the directory

#define directory where inputs parameters are stocked 
directoryToRefSimu = dirname(rstudioapi::getSourceEditorContext()$path)


library(sensobol)
library(foreach)
library(doParallel)
library(lubridate)

source(paste0(mainDir,'/functions/load.SurEau-Ecos.R'))   # load SuReau-Ecos                 


# following paths and files won't change throughout the sensitivity analysis
climateData_path  <- paste0(directoryToRefSimu,'/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
modeling_options  <- create.modeling.options(timeStepForEvapo=2,
                                             constantClimate=T,
                                             defoliation = F,
                                             compOptionsForEvapo = c("Fast"),
                                             thresholdMortality=88)
soilFile <- read.soil.file(filePath=paste0(directoryToRefSimu,'/Soil_test_champenoux.csv'))
vegFile  <- read.vegetation.file(filePath=  paste0(directoryToRefSimu,'/VegetationParams_ComparSurEauC.csv'),modeling_options=modeling_options)







