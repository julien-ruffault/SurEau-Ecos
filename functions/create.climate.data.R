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
#' format to run \code{run.SurEauR} Also check data consistency and input variables
#' according to modeling options (see \code{create.modeling.options} and simulation parameters (see \code{create.simulation.parameters)
#'
#' @param filePath the path of the input climate file.
#' @param climateData a dataframe 
#' @param modeling_options a list containing the modeling options created with \code{create.modeling.options}
#' @param simulation_parameters a list containing the simulation parameters with \code{create.simulation.parameters}
#'
#' @return
#' @export
#'
#' @examples
#' # create the list of modeling options
#' create.modeling.options()
#' # create  the list of simulation parameters
#' create.simulation.parameters(startYearSimulation = 1990, endYearSimulation = 1990)
create.climate.data <- function(filePath, climateData, modeling_options, simulation_parameters) {

  # Read file if it exists and climateData not provided, error otherwise------------------------------------
  
  if (missing(climateData)){
    if (file.exists(filePath)) {
    climate_data <- read.csv(filePath, dec = ".", sep = ";", header = T, stringsAsFactors = F)
    } else {
    stop(paste0("file : ", filePath), "' does not exist, check presence or spelling")
  }
    
} else {climate_data=climateData}
  
  
  # check climate_data variables and format ------------------------------------
  # (note : 'needccol' can be adjusted according to modeling_options)
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

  # checks NA's ----------------------------------------------------------------
  if (sum(is.na(climate_data)) > 1) {
    stop(paste0("remove NAs in file  : ", paste0("../", filePath), "' "))
  }

  # check the consistency of climate variables --------------------------------
  if (any(climate_data$PPT_sum < 0)) {
    stop(paste("Precipitation <0 mm in input climate file, that is a problem !"))
  }

  if (any(climate_data$RG_sum < 0)) {
    stop(paste("Global radiation <0 mm in input climate file, that is a problem !"))
  }

  if (any(climate_data$RHair_max > 100)) {
    climate_data[climate_data$RHair_max > 100, "RHair_max"] <- 100
    warning("several values of RHair_max were >100% in climate input file and were set to 100%.")
  }

  if (any(climate_data$RHair_mean > 100)) {
    climate_data[climate_data$RHair_mean > 100, "RHair_mean"] <- 100
    warning("Several values of RHair_mean were >100% in climate input file and were set to 100 %.")
  }

  if (any(climate_data$RHair_min > 100)) {
    climate_data[climate_data$RHair_min > 100, "RHair_min"] <- 100
    warning("Several values of RHair_min were >100% in the input climate data and were set to 100 %.")
  }

  if (any(climate_data$Tair_min > (climate_data$Tair_mean-0.5))) {
    climate_data[climate_data$Tair_min > (climate_data$Tair_mean-0.5), "Tair_min"] <-climate_data[climate_data$Tair_min > (climate_data$Tair_mean-0.5), "Tair_mean"] -0.5
    warning(" values of Tair_min were > Tair_mean in the input climate data and set to 'Tair_mean'")
  }
  
  if (any(climate_data$Tair_max < (climate_data$Tair_mean+0.5))) {
    climate_data[climate_data$Tair_max < (climate_data$Tair_mean+0.5), "Tair_max"] <-climate_data[climate_data$Tair_max < (climate_data$Tair_mean+0.5), "Tair_mean"] +0.5
    warning(" values of Tair_max were < Tair_mean in the input climate data and set to 'Tair_mean'")
  }
  
  
  # Select rows of climate_data according to simulation parameters--------------
  if (modeling_options$constantClimate == F) {
    # check and format  DATES
    if (any(names(climate_data) == "DATE")) {
      if (sum((is.na(as.Date(climate_data$DATE, "%d/%m/%Y")))) > 1) {
        stop("variable DATE is not in the right format in the input climate file, should be as 'dd/mm/yyyy' : e.g. '01/01/1999'")
      }
      else {
        climate_data$DATE <- as.Date(climate_data$DATE, format = "%d/%m/%Y")
        climate_data$Doy <- yday(climate_data$DATE)
        climate_data$Day <- day(climate_data$DATE)
        climate_data$Month <- month(climate_data$DATE)
        climate_data$Year <- year(climate_data$DATE)
      }
    } else {
      stop(paste("no variable 'DATE' in the input climate file!"))
    }
    io <- climate_data$Year >= simulation_parameters$startYearSimulation & climate_data$Year <= simulation_parameters$endYearSimulation

    if (sum(io) == 0) {
      stop("dates in input climate file and simulation parameters do not match")
    } else {
      climate_data <- climate_data[io, ]
      print(paste0(sum(io), " days were selected in the input climate file , covering the period : ", min(climate_data$Year), " - " , max(climate_data$Year)))
      
    }
  }

  # select period in input climate data ---------------------------------------
  if (modeling_options$constantClimate == T) {
    dateref <- seq.Date(
      from = as.Date(paste0("01/01/", simulation_parameters$startYearSimulation), format = "%d/%m/%Y"),
      to = as.Date(paste0("31/12/", simulation_parameters$endYearSimulation), format = "%d/%m/%Y"),
      by = "days"
    )
    climate_data <- as.data.frame(lapply(climate_data[1, ], rep, length(dateref)))
    climate_data$DATE <- dateref
    climate_data$Doy <- yday(dateref)
    climate_data$Day <- day(dateref)
    climate_data$Month <- month(dateref)
    climate_data$Year <- year(dateref)
  }
  return(climate_data)
}
