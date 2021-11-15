
#' Create a list with simulation parameters to run SureauEcos. Can be used as an
#' input in
#'
#' @param mainDir
#' @param startYearSimulation a numeric indicating the starting year for the
#'   simulation (must match the dates of the input climate data file)
#' @param endYearSimulation a numeric indicating the starting year for the
#'   simulation (must match the dates of the input climate data file)
#' @param resolutionOutput the resolution chosen to write variables in files,
#'   'subdaily' (default), 'daily' or 'yearly'.
#' @param outputType the output variables of the model that should be written in
#'   the output model file.
#' @param outputPath the path of  output result file.
#' @param overWrite a logical value (T or F) indicating whether the output
#'   result file can be overwritten if it already exists (default = F)
#'
#' @return
#' @export
#'
#' @examples
#' create.simulation.parameters(StartYearSimulation = 1990, EndYearSimulation = 1990)
create.simulation.parameters <- function(mainDir,
                                         startYearSimulation,
                                         endYearSimulation,
                                         resolutionOutput = "subdaily",
                                         overWrite = F,
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
  if (!resolutionOutput %in% c("none","subdaily", "daily", "yearly")) {
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
    stop(paste0("'overWrite' must be logical not ", typeof(overWrite), "."))
  }
  if (overWrite == F & file.exists(outputPath)) {
    stop("file already exists and 'overWrite' option is set to False, change 'outputPath' or set 'overWrite' to T.")
  }

  if (!missing(startYearSimulation) & !missing(endYearSimulation)) {
    if (!is.numeric(startYearSimulation) | !is.numeric(endYearSimulation)) {
      stop(" 'startYearSimulatin' and 'endYearSimulation' must be numeric.")
    }
    if (startYearSimulation > endYearSimulation) {
      stop(" 'startYearSimulation' must be < or = to 'endYearSimulation'.")
    }
  } else {
    stop("'startYearSimulation' and/or 'endYearSimulation' are missing.")
  }


  simulation_parameters$startYearSimulation <- startYearSimulation
  simulation_parameters$endYearSimulation <- endYearSimulation

  return(simulation_parameters)
}
