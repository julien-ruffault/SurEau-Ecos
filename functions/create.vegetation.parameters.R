## ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create list with all species parameters from configuration file
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

#' create a list wit the vegetation parameters to run \code{run.SurEauR}
#'
#' @param filePath path to a csv file containing parameter values 
#' @param stand_parameters  a list containing stand parameters created with \code{create.stand.parameters} 
#' @param modeling_options  a list containing modeling options created with \code{create.modeling.options} 
#'
#' @return
#' a list with modeling options to run  SurEau-Ecos
#' @export
#' @examples
#'
create.vegetation.parameters <- function(filePath, stand_parameters, soil_parameters, modeling_options) {

  if (file.exists(filePath)) {
    io <- read.csv(file = filePath, sep = ";", dec='.',head = T)
  } else {
    stop(paste0("Could not find input parameter file : ", filePath))
  }
  
  colnames(io) <- c("Name", "Value")

  .veg_params <- list() # initialization 
  .veg_params$LAImax <- stand_parameters$LAImax
  
  # setting commomn params for WB_veg (regardless of the options)
  params <- c(
    "P50_VC_TL", # [MPa] / Water potential causing 50% Cavitation in the vulnerability curve
    "slope_VC_TL", # [%/MPa]             / Slope of the vulnerability curve
    "P50_VC_Root",
    "slope_VC_Root",
    "EpsilonSymp_Leaf", # [MPa]            / Modulus of elasticity in leaves
    "PiFullTurgor_Leaf", # [MPa]          / Osmotic Potential at full turgor in leaves
    "ApoplasmicFrac_Leaf", # [-]           / Apoplasmic Fraction in leaves
    "LDMC", # [mgMS/g]                / Leaf dry matter content (measured for fully watered leaves)
    "LMA", # [g/m2leaf]                   / Leaf mass per area
    "K", # [-]                        / Light extinction coefficient of the vegetation layer
    "SapwoodVolume", # [m3]           / Sapwood volume   / unused [per soil surface ? )
    "WoodDensity", # [-]                / Wood density    /  unused
    "kPlantInit", # [mmol/MPa/s/m2leaf]  / Hydaulic conductance of the plant from soil to leaves
    "gmin20", # [mmol/m2leaf/s]         / Minimum conductance (gmin) at the reference temperature
    "TPhase_gmin", # [degC]            / Temperature for phase transition of minimum conductance
    "Q10_1_gmin", # [-]                 / Q10 value for gmin = f(T) <= Tphase_gmin
    "Q10_2_gmin", # [-]                 / Q10 value for gmin = f(T)  > Tphase_gmin
    "gmin_T",  #  conductance (gmin) of the trunk
    "CanopyStorageParam", # [l/m2leaf]    / Depth of water that can be retained by leaves and trunks per unit of leaf area index (used to compute the canopy water storage capacity as a function of LAI)
    "k_TSymInit",
    "gCrown0", 
    "gsMax",      # parameter in Jarvis model (currently default to 200) 
    "gsNight",    # parameter in jarvis gs model (currently default to 20 
    "JarvisPAR",  # parameter in Jarvis gs mdoel (currently default is 0.006) 
    "Tgs_sens",   # temperature parameter in Jarvis model (currently default Value is 17)
    "Tgs_optim",  # temperature parameter in Jarvis model (currently default value is 25)
    "fRootToLeaf", # root to leaf ratio 
    "rootRadius",  #  radius of roots (m)
    "betaRootProfile", # parameter for the distribution of roots in the soil 
    "PiFullTurgor_Trunk",
    "EpsilonSymp_Trunk",
    "ApoplasmicFrac_Trunk",
    "SymplasmicFrac_Trunk",
    "VolumeLiving_TRB",
    "fTRBToLeaf",
    "C_LApoInit",
    "C_TApoInit"
  )
  
  for (i in 1:length(params)) {
    AAA <- which(io$Name == params[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file/otherwise stop running
    {
      stop(paste0("'", params[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", filePath))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params[i], "' is not provided several times in input vegetation parameter file, correct \n", filePath))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0(".veg_params$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params[i], "must be numeric"))
      }
    }
  }
  
  
  
  # Gestion des parameters de vegetaion pour la regulation stomatique selon  les options 
  if (modeling_options$stomatalRegulationType=='PiecewiseLinear')
  {
    params_regulation <- c("PsiStartClosing", "PsiClose")
  } 
  if(modeling_options$stomatalRegulationType=='Sigmoid') {
  params_regulation <- c("P12_gs", "P88_gs")
  }
  
  for (i in 1:length(params_regulation)) {
    AAA <- which(io$Name == params_regulation[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file/otherwise stop running
    {
      stop(paste0("'", params_regulation[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", filePath))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params_regulation[i], "' is provided several times in input vegetation parameter file, correct \n", filePath))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0(".veg_params$", params_regulation[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params_regulation[i], "must be numeric"))
      }
    }
  }
  
  if (modeling_options$stomatalRegulationType=='Sigmoid')
  {.veg_params$P50_gs  = (.veg_params$P12_gs + .veg_params$P88_gs)/2
  .veg_params$slope_gs = 100/(.veg_params$P12_gs-.veg_params$P88_gs)
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
  
  
  
  # calculate root distribution within each soil layer (Jackson et al. 1996)
  .veg_params$rootDistribution <-numeric(3)
  .veg_params$rootDistribution[1] = 1-.veg_params$betaRootProfile^(soil_parameters$depth[1]*100) # conversion of depth to cm 
  .veg_params$rootDistribution[2] = (1-.veg_params$betaRootProfile^(soil_parameters$depth[2]*100))-.veg_params$rootDistribution[1]
  .veg_params$rootDistribution[3] = 1-(.veg_params$rootDistribution[1]+.veg_params$rootDistribution[2] )
  

  # determine root lenght (La gardner cowan)
  RAI = .veg_params$LAImax*.veg_params$fRootToLeaf
  
  .veg_params$La = RAI*.veg_params$rootDistribution / (2*pi*.veg_params$rootRadius)
  .veg_params$Lv = .veg_params$La/(soil_parameters$layer_thickness*(1-soil_parameters$rock_fragment_content/100))
  
  ##### calculate the different conductance of the plant from kPlantInit 
  conduc <- distribute.conductances(kPlantInit=.veg_params$kPlantInit, ri = .veg_params$rootDistribution) 
  .veg_params$k_TLInit <- conduc$k_TLInit
  .veg_params$k_RootInit <- conduc$k_RootInit 
  .veg_params$k_LSymInit <- conduc$k_LSymInit
  
  
  return(.veg_params)
}


