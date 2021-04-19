## ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create list with all species parameters from configuration file
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

#' create a list wit the vegetation parameters to run SureauR
#'
#' @param filePath path to a csv file containing parameter values (add a link for details) 
#' @param stand_parameters  a list containing stand parameters created with \code{create.stand.parameters} 
#' @param modeling_options  a list containing modeling options created with \code{create.modeling.options} 
#'
#' @return
#' @export
#'
#' @examples
create.vegetation.parameters <- function(filePath, stand_parameters, modeling_options) {

  if (file.exists(filePath)) {
    io <- read.csv(file = filePath, sep = ";", dec=',',head = T)
  } else {
    stop(paste0("Could not find input parameter file : ", filePath))
  }
  
  colnames(io) <- c("Name", "Value")

  .veg_params <- list() # initialization 
  .veg_params$LAImax <- stand_parameters$LAImax
  
  # setting commomn params for WB_veg (regardless of the options)
  params <- c(
    "P50_VC", # [MPa] / Water potential causing 50% Cavitation in the vulnerability curve
    "Slope_VC", # [%/MPa]             / Slope of the vulnerability curve
    "EpsilonSymp", # [MPa]            / Modulus of elasticity in leaves
    "PiFullTurgor", # [MPa]          / Osmotic Potential at full turgor
    "ApoplasmicFrac", # [-]           / Apoplasmic Fraction
    "LDMC", # [mgMS/g]                / Leaf dry matter content (measured for fully watered leaves)
    "LMA", # [g/m2leaf]                   / Leaf mass per area
    "K", # [-]                        / Light extinction coefficient of the vegetation layer
    "SapwoodVolume", # [m3]           / Sapwood volume   / unused [per soil surface ? )
    "WoodDensity", # [-]                / Wood density    /  unused
    "kPlantInit", # [mmol/MPa/s/m2leaf]  / Hydaulic conductance of the plant from soil to leaves
    "gmin20", # [mmol/m2leaf/s]         / Minimum conductance (gmin) at the reference temperature
    "TPhase_gmin", # [degC]            / Temperature for phase transition of minimum conductance
    "Q10_1_gmin", # [-]                 / Q10 value for gmin = f(T) <= Tphase_gmin
    "Q10_2_gmin", # [-]                 / Q10 value for gmin = f(T)  > Tphase_gmin``
    "CanoStorageParam", # [l/m2leaf]    / Depth of water that can be retained by leaves and trunks per unit of leaf area index (used to compute the canopy water storage capacity as a function of LAI)
    "PsiClose", # [MPa]
    "PsiStartClosing" # [MPA]
  )
  
  # "Lv_root", # [m/m3soil]             / Root length per unit soil volume
  # "r_root" # [m]                      / Root radius
  

  for (i in 1:length(params)) {
    AAA <- which(io$Name == params[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
    {
      stop(paste0("'", params[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", file))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params[i], "' is not provided several times in input vegetation parameter file, correct \n", file))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0(".veg_params$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params[i], "must be numeric"))
      }
    }
  }

  ##### Foliage Type  ####
  AAA <- io[which(io$Name == "Foliage"), "Value"]
  if (length(AAA) == 0) # checking that it exists in input file  /otherwise stop running
  {
    stop(paste0("'Foliage' is not provided in input vegetation parameter file, check presence or spelling"))
  } else if (length(AAA) > 1) {
    stop(paste0("'", params[i], "'Foliage' is provided several times in input vegetation parameter file, correct"))
  } else if (length(AAA) == 1) { # checking that parameter is numeric in input file /stop running otherwise
    .veg_params$Foliage <- as.character(AAA)
  }
  
  if (!.veg_params$Foliage %in% c("Evergreen", "Deciduous")) {
    stop("'Foliage' should be 'Evergreen' or 'Deciduous' / Check Type of spelling ")
  }
  
  if (.veg_params$Foliage == "Deciduous") {
    params_Decid <- c("Tbase", "Fcrit", "DayStart", "nbdayLAI")
    for (i in 1:length(params_Decid))
    {
      AAA <- which(io$Name == params_Decid[i]) ## line number of the variable
      
      if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
      {
        stop(paste0("'", params_Decid[i], "' is not provided in input vegetation parameter file, check presence or spelling"))
      } else if (length(AAA) > 1) {
        stop(paste0("'", params_Decid[i], "' is not provided several times in input vegetation parameter file, correctr"))
      } else if (length(AAA) == 1) {
        if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
          eval(parse(text = paste0(".veg_params$", params_Decid[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_Decid[i], "must be numeric"))
        }
      }
    }
  }
  
  
  
  # ETP parameters for PT or PM
  if (modeling_options$ETPFormulation == "PT") {
    params_PT <- c("PTcoeff")
    for (i in 1:length(params_PT))
    {
      AAA <- which(io$Name == params_PT[i]) ## line number of the variable
      
      if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
      {
        stop(paste0("'", params_PT[i], "' is not provided in input vegetation file, check presence and/or spelling"))
      } else if (length(AAA) > 1) {
        stop(paste0("'", params_PT[i], "' is provided several times in input vegetation file"))
      } else if (length(AAA) == 1) {
        if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
          eval(parse(text = paste0(".veg_params$", params_PT[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_PT[i], "must be numeric, check in input vegetation file "))
        }
      }
    }
  }
  
  return(.veg_params)
}


