#' Create a list containing modeling options that can be used as an input  
#' run_SurEau_Ecos
#'
#' @param timeStepForEvapo a numerical value (1,2,3,4 or 8) indicating the time
#'   step for the main evapotranspiration loop (in hours). Should be one of the
#'   following (default =1)
#' @param resetSWC a logical value indicating whether soil layers should be
#'   refilled at the beginning of each year of the simulation (default=F)
#' @param avoidWaterSoilTransfer a logical value indicating whether the transfer of water between soil layers should be avoided by disconnecting the soil layers that get refilled from the soil-plant system (default =F)
#' @param ETPFormulation  the formulation of ETP to be used, either 'PT' (Priestley-Taylor) or 'PM' (penmman). Default is 'PT'(Priestley Taylor)
#' @param RnFormulation the method to be used to calculate net radiation from global radiation 
#' @param constantClimate  a logical value indicating whether the c
#' @param compOptionsForEvapo the option to be used for ... (voir avec Francois) 
#'
#' @return
#' @export
#'
#'@examples
#'create_SurEau_modeling_options()
#'create_SurEau_modeling_options(timeStepForEvapo = 2, resetSWC = 2)
create.modeling.options <- function(timeStepForEvapo = 1,
                                           resetSWC = F,
                                           avoidWaterSoilTransfer = T,
                                           ETPFormulation = c("PT", "PM"),
                                           RnFormulation = c("Linacre", "Linear"),
                                           constantClimate = F,
                                           compOptionsForEvapo = c("Normal", "Accurate", "Special", "Fast", "Fast1")) {
  if (timeStepForEvapo == "Variable") {
    TIME <- c(6, 10, 12, 14, 18, 24)
    print("time step for evapotranspiration is variable and set to 6/12/14/18/24")
  } else if (as.numeric(timeStepForEvapo) %in% c(0.1, 0.2, 0.5, 1, 2, 4, 6, 8)) {
    TIME <- seq(as.numeric(timeStepForEvapo), 24, as.numeric(timeStepForEvapo))
  } else {
    stop(paste0("`timeStepForEvap` must be equal to 1,2,4,6 or 8  or set to 'variable'"))
  }

  if (!is.logical(resetSWC)) {
    stop("'resetSWC' must be set to 'T' or 'F'.")
  }

  if (!is.logical(avoidWaterSoilTransfer)) {
    stop(" 'avoidWaterSoilTransfer' must be 'T' or 'F'.")
  }

  if (!is.logical(constantClimate)) {
    stop(" 'constantClimate' must be 'T' or 'F'.")
  }

  ETPFormulation <- match.arg(ETPFormulation)
  RnFormulation <- match.arg(RnFormulation)

  compOptionsForEvapo <- match.arg(compOptionsForEvapo)
  if (compOptionsForEvapo == "Normal") {
    compOptions <- list("nsmalltimesteps" = c(6, 10, 20, 60), "Lsym" = 1, "Tsym" = 1, "Eord" = 1, "Lcav" = 1, "Tcav" = 1, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Accurate") {
    compOptions <- list("nsmalltimesteps" = c(180), "Lsym" = 1, "Tsym" = 1, "Eord" = 1, "Lcav" = 1, "Tcav" = 1, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Special") {
    compOptions <- list("nsmalltimesteps" = c(720), "Lsym" = 1, "Tsym" = 1, "Eord" = 1, "Lcav" = 1, "Tcav" = 1, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Fast") {
    compOptions <- list("nsmalltimesteps" = c(1, 6), "Lsym" = 1, "Tsym" = 1, "Eord" = 1, "Lcav" = 1, "Tcav" = 1, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Fast1") {
    compOptions <- list("nsmalltimesteps" = c(1), "Lsym" = 1, "Tsym" = 1, "Eord" = 1, "Lcav" = 1, "Tcav" = 1, "CLapo" = 1, "CTapo" = 1)
  }

  modeling_options <- list()
  modeling_options$constantClimate <- constantClimate
  modeling_options$ETPFormulation <- ETPFormulation
  modeling_options$RnFormulation <- RnFormulation
  modeling_options$timeStepForEvapo <- timeStepForEvapo
  modeling_options$TIME <- TIME
  modeling_options$resetSWC <- resetSWC
  modeling_options$avoidWaterSoilTransfer <- avoidWaterSoilTransfer
  modeling_options$compOptions <- compOptions

  return(modeling_options)
}
