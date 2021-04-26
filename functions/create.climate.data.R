# variables,format and unit of the input climate data file (see documentation)
#  - DATE      : 'dd/mm/YY' (if modeling_optionsconstantClimate==F)
#  -Tair_min   : minimum air temperature of the day (degC)
# - Tair_max   : maximum air temperature of the day (degC)
# - Tair_mean  : mean air temperature of the day (degC)
# - RG_sum     : global radiation (MJ/m2)
# - PPT_sum    : precipitation (mm)
# - RHair_min  : minimum relative humidity of the day (%)
# - RHAir_max  : maixmum relative humidity of the day (%)
# - RHair_mean : mean relative humidity of the day (%)
# - WS_mean    : mean wind speed of the day (m/s)

#' Create a climate data.frame to run SureauR
#' read input climate data /selec the desired period and put it in the right
#' format to run SurEau_Ecos Also check data consistency and input variables
#' according to modeling_options and simulation parameters created in
#' create_Sureau_modeling_options and create_SurEau_simuation_parameters
#' 
#' @param filePath the path of the input file containing climate data
#' @param modeling_options the list containing the modeling options created with the function create_SurEau_modeling options 
#' @param simulation_parameters the list containing the simulation parameters with the function create_SurEau_Simulation_parameters
#'
#' @return
#' @export
#'
#' @examples
#' # create the list of modeling options
#' create.modeling.options()
#' # create  the list of simulation parameters
#' create.simulation.parameters(startYearSimulation = 1990, endYearSimulation = 1990)
create.climate.data <- function(filePath, modeling_options, simulation_parameters)
{
  # Read file if it exists, error otherwise
  if (file.exists(filePath)) {
    climate_data <- read.csv(filePath, dec = ",", sep = ";", header = T, stringsAsFactors = F)
  }else{stop(paste0("file : ", filePath), "' does not exist, check presence or spelling")}
  
  # check that the climate_data contains all necessary variables and that they are all numeric ('needccol' can be adjusted according to  modeling_options)
  needcol <- c("Tair_min", "Tair_max", "Tair_mean", "RG_sum", "PPT_sum", "RHair_min", "RHair_max", "RHair_mean", "WS_mean")
  for (i in needcol) {
    if (any(names(climate_data) == i)) {
      if (!is.numeric(climate_data[, which(names(climate_data) == i)])) {
        stop(paste0("oups, variable : -", i, "- not numeric in input climate file, please check data or input format"))
      }
    }
    else {
      stop(paste0("varaible : -", i, "- not found in input climate file, please check presence/spelling"))
    }
  }
  

  # checks NA's
  if (sum(is.na(climate_data)) > 1) {
    stop(paste0("remove NAs in file  : ", paste0("../", filePath), "' "))
  }
  
 
  
  # check the consistency of climate variables
  # PPT < 0
  if (any(climate_data$PPT_sum < 0)) {
    stop(paste("Precipitation <0 mm in input climate file, that is a problem !"))
  }
  
  # RG < 0
  if (any(climate_data$RG_sum < 0)) {
    stop(paste("Global radiation <0 mm in input climate file, that is a problem !"))
  }
  
  # RH > 100
  if (any(climate_data$RHair_max > 100)) {
    climate_data[climate_data$RHair_max > 100, "RHair_max"] <- 100
    warning('several values of RHair_max were >100% in climate input file')
  }
  
  if (any(climate_data$RHair_mean > 100)) {
    climate_data[climate_data$RHair_mean > 100, "RHair_mean"] <- 100
    warning('several values of RHair_mean were >100% in climate input file')
  }
  
  if (any(climate_data$RHair_min > 100)) {
    climate_data[climate_data$RHair_min > 100, "RHair_min"] <- 100
    warning('several values of RHair_min were >100% in climate input file')
  }
  
  
  # Select rows of climate_data according to simulation parameters and check that / stop code if nrows do not match 
    if(modeling_options$constantClimate==F)
    {
      # check and format  DATES
      if (any(names(climate_data) == "DATE")) {
        if (sum((is.na(as.Date(climate_data$DATE, "%d/%m/%Y")))) > 1) {
          stop("variable DATE is not in the right format in the input climate file, should be as 'dd/mm/yyyy' : e.g. '01/01/1999'")
        }
        else {
          climate_data$DATE <- as.Date(climate_data$DATE, format = "%d/%m/%Y")
          climate_data$Doy <- yday(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
          climate_data$Day <- day(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
          climate_data$Month <- month(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
          climate_data$Year <- year(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
        }
      
       # io = match(climate_data$DATE, simulation_parameters$timeDateSimulation)
        io = climate_data$DATE %in% simulation_parameters$timeDateSimulation
        climate_data = climate_data[io,]
      #  na.omit(climate_data)
        
        } else {
        stop(paste("no variable 'DATE' in the input file!"))
      }
      
      
      
      
      
      }

    if (modeling_options$constantClimate == T) {
      climate_data <- as.data.frame(lapply(climate_data[1,], rep, length(simulation_parameters$timeDateSimulation )))
      climate_data$DATE <- simulation_parameters$timeDateSimulation 
      climate_data$Doy <- yday(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Day <- day(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Month <- month(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Year <- year(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
    }  
    
    

  return(climate_data)
}