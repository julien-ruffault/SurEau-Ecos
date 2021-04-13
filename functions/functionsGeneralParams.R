### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Authors : Julien Ruffault (julien.ruff@gmail.com)
#                  &
#           Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#                  &
#           Francois Pimont (francois.pimont@inrae.fr)
#           date   : 30/03/2020 (V0)
#                  : 26/11/2020 (V1) update create_soil_params to read from file
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

create.general.params <- function(model_path,
                                  soil_path,
                                  climate_path,
                                  time_mod,
                                  start_year,
                                  end_year,
                                  species,
                                  fileNameInfo,
                                  addInfotoFileName = F,
                                  lat,
                                  long,
                                  LAImax,
                                  outputType,
                                  calcul_Emin = T,
                                  calcul_Tleaf = T,
                                  reset_SWC = F,
                                  LAImod = T,
                                  avoidWaterSoilTransfer = T,
                                  ETP.formulation = "PT",
                                  Rn.formulation = "Linacre",
                                  timeStepForEvapo = 1,
                                  constantClimate=F,
                                  EboundComp =1
                                  ) {


  #
  if (timeStepForEvapo == "Variable") {
    TIME <- c(6, 10, 12, 14, 18, 24)
    print("time step for evapotranspiration is variable and set to 6/12/14/18/24")
  } else if (as.numeric(timeStepForEvapo) %in% c(0.1, 0.2, 0.5, 1, 2, 4, 6, 8, 12, 24)) {
    TIME <- seq(as.numeric(timeStepForEvapo), 24, as.numeric(timeStepForEvapo))
  } else {
    stop(paste0("`timeStepForEvap` must be equal to 1,2,4,6,8,12 or 24 (daily) or set to 'variable'"))
  }


  if (missing(model_path)) {
    warning("'model_path' is missing and set to 'getwd()' (Default option).")
    model_path=getwd()
  }

  if (missing(soil_path)) {
    stop("'soil_path' is missing.")
  }

  if (missing(climate_path)) {
    stop("'climate_path' is missing.")
  }

  if (missing(start_year) & missing(time_mod)) {
    stop("'start_year' and 'time_mod' are missing, at least one of the two must be entered.")
  }
  if (missing(end_year) & missing(time_mod)) {
    stop("'end_year'is missing and 'time_mod' are missing, at least one of the two must be entered.")
  }


  if (!missing(start_year) & missing(end_year)) {
    if (!is.numeric(start_year) | !is.numeric(end_year)) {
      stop(" start_year and end_year must be numeric.")
    }
    if (start_year > end_year) {
      stop(" start_year must <= end_year.")
    }
  }
  
  if (!is.character(model_path)) {
    stop(paste0("`base` must be character not ", typeof(model_path), "."))
  }
  if (!file.exists(model_path)) {
    stop(paste0("'model_path:'", model_path, "' does not exist or cannot be reached."))
  }

  if (!ETP.formulation %in% c("PT", "PM")) {
    stop("ETP.formulation should be 'PT' or PM'.")
  }
  if (!Rn.formulation %in% c("Linacre", "Linear")) {
    stop("Rn formulation should be 'Linacre' or 'Linear'.")
  }
  if (!is.logical(LAImod)) {
    stop(paste0("`LAImod` must be logical (T or F) not ", typeof(LAImod), "."))
  }

  if (missing(model_path)) {
    stop("'model_path' is missing.")
  }

  if (missing(fileNameInfo)) {
    fileNameInfo <- "Default_file_name"
    print("Warning  : 'fileNameinfo' was not provided ans set to 'default'.")
  }

  if (!is.character(fileNameInfo)) {
    stop(paste0("`fileNameInfo' must be character not ", typeof(fileNameInfo), "."))
  }


  if (missing(LAImax)) {
    stop("'LAImax' is missing.")
  }
  if (!is.numeric(LAImax)) {
    stop("LAImax must be numeric.")
  }

  if (missing(calcul_Emin)) {
    print("'calcul_Emin' is missing and set to 'T'.")
    calcul_Emin <- T
  }

  
  if (missing(EboundComp)) {
    print("'EboundComp' is missing and set to '1'.")
    EboundComp <- 1
  }
  
  
  if (EboundComp==2  && calcul_Tleaf==F) {
    stop("'EboundComp' cannnot be set to 2 if calcul Tleaf is set to 'F'")
  }
  
  
  
  
  if (missing(species)) {
    stop("'species' is missing.")
  }

  if (!is.character(species)) {
    stop(paste0("`species` must be character not ", typeof(Species), "."))
  }

  if (!is.logical(calcul_Emin)) {
    stop(" calcul_Emin must be 'T' or 'F'.")
  }

  if (!is.logical(calcul_Tleaf)) {
    stop(" calcul_Tleaf must be 'T' or 'F'. ")
  }

  if (!is.logical(reset_SWC)) {
    stop(" calcul_Tleaf must be 'T' or 'F'.")
  }

  if (!is.logical(avoidWaterSoilTransfer)) {
    stop(" 'avoidWaterSoilTransfer' must be 'T' or 'F'.")
  }


  if (!is.logical(addInfotoFileName)) {
    stop(" 'addTimetoFileName' must be 'T' or 'F'.")
  }

  if (!is.numeric(LAImax)) {
    stop(" LAImax must be numeric ")
  }



  general_params <- list() # initialisation
  general_params$model_path <- model_path


  if (!missing(time_mod)) {
    if (sum((is.na(as.Date(time_mod, "%d/%m/%Y")))) > 1) {
      stop(" 'time_mod' is not in the right format, should be as 'dd/mm/yyyy' : e.g. '01/01/1999'")
    }
    general_params$time_mod <- time_mod
  } else if (missing(time_mod)) {
    general_params$start_year <- start_year
    general_params$end_year <- end_year
    general_params$time_mod <- seq.Date(
      from = as.Date.character((paste0(start_year, "/01/01"))),
      to = as.Date.character((paste0(end_year, "/12/31"))),
      by = "days"
    )
  }
  
  general_params$soil_path <- soil_path

  general_params$species <- species

  general_params$lat <- lat
  general_params$long <- long
  general_params$LAImax <- LAImax
  general_params$outputType <- outputType

  general_params$EboundComp <- EboundComp
  general_params$constantClimate <- constantClimate

  general_params$LAImod <- LAImod
  general_params$ETP.formulation <- ETP.formulation
  general_params$Rn.formulation <- Rn.formulation
  general_params$timeStepForEvapo <- timeStepForEvapo
  general_params$TIME = TIME


  general_params$fileNameInfo <- fileNameInfo
  general_params$calcul_Emin <- calcul_Emin
  general_params$calcul_Tleaf <- calcul_Tleaf

  general_params$reset_SWC <- reset_SWC
  general_params$avoidWaterSoilTransfer <- avoidWaterSoilTransfer
  general_params$addInfotoFileName <- addInfotoFileName

  
  general_params$climate_path <- climate_path
  


  return(general_params)
}
