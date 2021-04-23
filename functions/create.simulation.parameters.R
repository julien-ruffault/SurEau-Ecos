
#' create simulation parameters for SureauR
#'
#' @param mainDir 
#' @param timeDateSimulation a vector with the dates used as reference dates for the simulation. see
#' @param startYearSimulation a starting year for the simulation 
#' @param endYearSimulation last year of simulation 
#' @param resolutionOutput the resolution chosen to write variables in files 
#' @param outputType the type of outputs that should be written. see details 
#' @param outputFileName the name of the output file. 
#' @param addInfotoFileName a logical value indicating whether 
#'
#' @return
#' @export
#'
#' @examples
create.simulation.parameters <- function(mainDir,
                                         timeDateSimulation,
                                         startYearSimulation,
                                         endYearSimulation,
                                         resolutionOutput = "subdaily",
                                         overWrite=F,
                                         outputType,
                                         outputPath) {
  simulation_parameters <- list()

  if (!missing(mainDir)) {
    if (!is.character(mainDir)) {
      stop(paste0("`mainDir` must be character not ", typeof(mainDir), "."))
    }
    if (!file.exists(mainDir)) {
      stop(paste0("'mainDir:'", mainDir, "' does not exist or cannot be reached."))
    }
    simulation_parameters$mainDir <- mainDir
  } else {
    stop("'mainDir' is missing.")
  }
  if (!resolutionOutput %in% c("subdaily", "daily", "yearly")) {
    error("resolutionOutput should be set 'subdaily (default), 'daily' 'yearly'")
  }

  simulation_parameters$resolutionOutput <- resolutionOutput

  if (missing(outputType)) {
    if (resolutionOutput == "subdaily") {
      simulation_parameters$outputType <- "simple_subdaily"
      print("'outputType' is not provided and set to default : 'simple_subdaily'. See how to create cumstom output and other options in help.")
    }
    else if (resolutionOutput == "daily") {
      simulation_parameters$outputType <- "simple_daily"
      print("'outputType' is not provided and set to default : 'simple_daily'. See how to create cumstom output and other options in help.")
    }
    else if (resolutionOutput == "yearly") {
      simulation_parameters$outputType == "simple_yearly"
      print("'outputType' is not provided and set to default : 'simple_yearly'. See how to create cumstom output and other options in help.")
    }
  }


  if (!missing(outputType)) {
    if (!is.character(outputType)) {
      stop(paste0("`outputType` must be character not ", typeof(outputType), "."))
    } else {
      simulation_parameters$outputType <- outputType
    }
  }

  if (!missing(outputPath)) {
    if (!is.character(outputPath)) {
      stop(paste0("`outputPath' must be character not ", typeof(fileNameInfo), "."))
    }
    simulation_parameters$outputPath <- outputPath
  } else {
    stop("'outputPath' is missing with no default value ")
  }
  
  if (!is.logical(overWrite)) {
    stop(paste0("'overWrite' must be logical not ", typeof(overWrite), "."))}
  if(overWrite==F & file.exists(outputPath)){stop("file already exists and 'overWrite' option is set to False, change 'outputPath' or set 'overWrite' to T.")}
  

  if (missing(startYearSimulation) & missing(timeDateSimulation)) {
    stop("'startYearSimulation' and 'refDateSimulation' are missing, at least one of the two must be entered.")
  }
  if (missing(endYearSimulation) & missing(timeDateSimulation)) {
    stop("'endYearSimulation'is missing and 'refDateSimulation' are missing, at least one of the two must be entered.")
  }
  if (!missing(startYearSimulation) & !missing(endYearSimulation)) {
    if (!is.numeric(startYearSimulation) | !is.numeric(endYearSimulation)) {
      stop(" 'startYearSimulatin' and 'endYearSimulation' must be numeric.")
    }
    if (startYearSimulation > endYearSimulation) {
      stop(" 'startYearSimulation' must be < or = to 'endYearSimulation'.")
    }
  }
  if (!missing(timeDateSimulation)) {
    if (sum((is.na(as.Date(time_mod, "%d/%m/%Y")))) > 1) {
      stop(" 'timeDateSimulation' is provide but is not in the right format, should be as 'dd/mm/yyyy' : e.g. '01/01/1999'")
      simulation_parameters$timeDateSimulation <- as.Date(timeDateSimulation, format = "%d/%m/%Y")
      simulation_parameters$startYearSimulation <- min(year(timeDateSimulation))
      simulation_parameters$endYearSimulation <- max(year(timeDateSimulation))
    }
  } else {
    simulation_parameters$startYearSimulation <- startYearSimulation
    simulation_parameters$endYearSimulation <- endYearSimulation
    simulation_parameters$timeDateSimulation <- seq.Date(
      from = as.Date.character((paste0(startYearSimulation, "/01/01"))),
      to = as.Date.character((paste0(endYearSimulation, "/12/31"))),
      by = "days"
    )
  }
  return(simulation_parameters)
}
