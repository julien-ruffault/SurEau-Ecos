#' Create a list containing modeling options that can be used as an input
#' in \code{run.SurEauR}
#'
#'#' @param constantClimate  a logical value indicating whether the climate should
#'   be considered constant or not (default = F)
#' @param timeStepForEvapo a numerical value (in hours) indicating the time
#'   step for the main evapotranspiration loop. Should be one of the
#'   following 1,2,4,6,8 (default = 1). 
#'  @param compOptionsForEvapo the option to be used for the loops  (voir avec
#'   Francois)
#' @param resetSWC a logical value indicating whether soil layers should be
#'   refilled at the beginning of each year (default=F)
#' @param soilEvapo a logical value indicating whether soil evaporation should be 
#' simulated (T) or set to 0 (F) ,default=T
#' @param avoidWaterSoilTransfer a logical value indicating whether the transfer
#'   of water between soil layers should be avoided by disconnecting the soil
#'   layers that get refilled from the soil-plant system (default =F). Yet to be
#'   implemented 
#' @param ETPFormulation  the formulation of ETP to be used, either 'PT'
#'   (Priestley-Taylor) or 'P' (Penmman). Default is 'PT'.
#'   Note: Penmman formulation is not implemented yet
#' @param RnFormulation the method to be used to calculate net radiation from
#'   global radiation, either 'Linacre' (default) or 'Linear' . Note : the linear 
#'   method is not implemnted yet 
#' @param stomatalRegFormulation the type of regulation to be used for stomatal
#'   response to leaf symplasmic water potential, either 'Sigmoid' (default) or 
#'   'PiecewiseLinear'
#' @param defoliation a logical value indicating whether trees should loose 
#' leaves when occurs.cavitation occurs of the above part of plant.  Defoliation 
#' starts only when PLC_Leaf > 10% .
#' @param thresholdMortatliy a numeric value indicating the PLC value (in % ) 
#' above which the plant is considered dead and simulation stops for the current
#' year. Default value is 90 
#' @param numericalScheme the method to be used, either "Implicit" or "Semi-Implicit" (Xu)
#' 
#'
#' @return
#' @export
#'
#' @examples
#' create_SurEau_modeling_options()
#' create_SurEau_modeling_options(timeStepForEvapo = 2, resetSWC = 2)
create.modeling.options <- function(timeStepForEvapo = 1,
                                    resetSWC = F,
                                    avoidWaterSoilTransfer = T,
                                    soilEvap=T,
                                    defoliation =F,
                                    thresholdMortality = 90,
                                    transpirationModel = c('Jarvis','Granier'),
                                    ETPFormulation = c("PT", "Penman"),
                                    RnFormulation = c("Linacre", "Linear"),
                                    PedoTransferFormulation = c("VG", "Campbell"),
                                    constantClimate = F,
                                    compOptionsForEvapo = c("Normal", "Accurate","Fast", "Custom"),
                                    customSmallTimeStepInSec = 600,
                                    Lcav = 1,
                                    Scav = 1,
                                    Eord=1,
                                    numericalScheme = c("Implicit","Semi-Implicit","Explicit"),
                                    stomatalRegFormulation = c("Sigmoid","PiecewiseLinear", "Turgor"),
                                    printProg=T) {
  if (timeStepForEvapo == "Variable") {
    TIME <- c(0, 6, 12, 14, 16, 22)
    print("time step for evapotranspiration is variable and set to 6/12/14/18/24")
  } else if (as.numeric(timeStepForEvapo) %in% c(1, 2, 4, 6)) {
    TIME <- seq(0, 23, as.numeric(timeStepForEvapo))
  } else {
    stop(paste0("`timeStepForEvap` must be equal to 1,2,4,6 or set to 'variable'"))
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
  
  if (!is.logical(defoliation)) {
    stop(" 'defoliation' must be 'T' or 'F'.")
  }
  
  if (!is.logical(soilEvap)) {
    stop(" 'SoilEvap' must be 'T' or 'F'.")
  }
  
  if (!is.numeric(thresholdMortality)) {
    stop(" 'thresholdMortatlity' must be numeric.")
  }
  
  if (thresholdMortality>100  | thresholdMortality<50) {
    stop(" 'thresholdMortatlity' must be >50 and <100 .")
  }
  
  ETPFormulation <- match.arg(ETPFormulation)
  RnFormulation <- match.arg(RnFormulation)
  RnFormulation <- match.arg(RnFormulation)

  compOptionsForEvapo <- match.arg(compOptionsForEvapo)
  stomatalRegFormulation <- match.arg(stomatalRegFormulation)
  transpirationModel <-  match.arg(transpirationModel)

  numericalScheme <-  match.arg(numericalScheme)
  
  PedoTransferFormulation <- match.arg(PedoTransferFormulation)
  
  
  if (compOptionsForEvapo == "Normal") { # every 10 min, 6 min, 3min, 1min
    compOptions <- list("numericalScheme"=numericalScheme,"nsmalltimesteps" = timeStepForEvapo*c(6, 10, 20, 60), "Lsym" = 1, "Ssym" = 1, "Eord" = Eord, "Lcav" = Lcav, "Scav" = Scav, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Accurate") { # every 10 s
    compOptions <- list("numericalScheme"=numericalScheme,"nsmalltimesteps" = timeStepForEvapo*c(600), "Lsym" = 1, "Ssym" = 1, "Eord" = Eord, "Lcav" = Lcav, "Scav" = Scav, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Fast") { # every hours, every 10 min
    compOptions <- list("numericalScheme"=numericalScheme,"nsmalltimesteps" = timeStepForEvapo*c(1, 6), "Lsym" = 1, "Ssym" = 1, "Eord" = Eord, "Lcav" = Lcav, "Scav" = Scav, "CLapo" = 1, "CTapo" = 1)
  }
  if (compOptionsForEvapo == "Custom") { # every customSmallTimeStepInSec 
    compOptions <- list("numericalScheme"=numericalScheme,"nsmalltimesteps" = c(timeStepForEvapo*3600/customSmallTimeStepInSec), "Lsym" = 1, "Ssym" = 1, "Eord" = Eord, "Lcav" = Lcav, "Scav" = Scav, "CLapo" = 1, "CTapo" = 1)
  }
  

  modeling_options <- list()
  modeling_options$constantClimate <- constantClimate
  modeling_options$ETPFormulation <- ETPFormulation
  modeling_options$RnFormulation <- RnFormulation
  modeling_options$PedoTransferFormulation <- PedoTransferFormulation 
  modeling_options$timeStepForEvapo <- timeStepForEvapo
  modeling_options$TIME <- TIME
  modeling_options$resetSWC <- resetSWC
  modeling_options$avoidWaterSoilTransfer <- avoidWaterSoilTransfer
  modeling_options$compOptions <- compOptions
  modeling_options$stomatalRegFormulation <- stomatalRegFormulation
  modeling_options$soilEvap <- soilEvap
  modeling_options$defoliation <- defoliation
  modeling_options$thresholdMortatliy <- thresholdMortality
  modeling_options$transpirationModel <- transpirationModel
  modeling_options$printProg <- printProg
  
  return(modeling_options)
}
