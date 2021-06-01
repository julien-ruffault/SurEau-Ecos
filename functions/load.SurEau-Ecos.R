#load functions and libraries for SurEau-Ecos 
#Should be deleted for packaging 

# Required libraries----------------------------------------------------------
library(insol)
library(lubridate)

# load functions ----------------------------------------------------------
source(paste0(mainDir, "/functions/run.SurEau_Ecos.R"))  
source(paste0(mainDir, "/functions/functionsWBveg.R"))
source(paste0(mainDir, "/functions/functionsWBOutput.R"))
source(paste0(mainDir, "/functions/functionsWBsoil.R"))
source(paste0(mainDir, "/functions/functionsWBclim.R"))

source(paste0(mainDir, "/functions/create.modeling.options.R"))
source(paste0(mainDir, "/functions/create.simulation.parameters.R"))
source(paste0(mainDir, "/functions/create.climate.data.R"))
source(paste0(mainDir, "/functions/create.stand.parameters.R"))
source(paste0(mainDir, "/functions/create.soil.parameters.R"))
source(paste0(mainDir, "/functions/create.vegetation.parameters.R"))

source(paste0(mainDir, "/functions/climate.utils.R"))
source(paste0(mainDir, "/functions/leafandSoilFunctions.R"))
source(paste0(mainDir, "/functions/plant.utils.R"))